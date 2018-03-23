000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200890921     H*----------------------------------------------------*
000300000124     H*   CREA  ARCHIVIO BOLLE FATTURATE  AL CLIENTE       *
000400160412
000500160412     H* ATTENZIONE: in caso di riutilizzo modificare perchè
000600160412     c* TASNAS non è più da utilizzare!!!!
000700160412     c* sostituire con §TA4ANAS
000800160412
000900030516     H* relese *CURRENT  (TGTRLS)                          *
001000890921     H*----------------------------------------------------*
001100890921     F*
001200120510     FWURTH_758 iF   E             DISK
001300120510     F                                     RENAME(wurth_758:wurth)
001400120510     FTITAS30C  IF   E           K DISK
001500120510     FTITA730C  IF   E           K DISK
001600990817     FTITA430C  IF   E           K DISK
001700050905     FFNORM01L  IF   E           K DISK
001800051219     fAZORG01L  if   e           k disk
001900910830     FTABEL00F  IF   E           K DISK
002000040531     FFIAR531C  IF   E           K DISK
002100120510     Fwfvaf00f  uf A E             DISK
002200030515     D VALNUM          S             10    DIM(1) CTDATA PERRCD(10)
002300050906     D VSV             S              1    DIM(30)                              SIGLA VARIE
002400990910     D VVA             S             11  3 DIM(30)                              IMPOR VARIE
002500120510     D DS4C          E DS
002600050906     D DSQT1         E DS
002700050906     D DS$2          E DS
002800030516     D DTA4A         E DS
002900120510     D DTA44         E DS
003000050905      *
003100010402     D*------------------
003200010402     D* DS di ridefinizione estensione riferimenti bolle (TITA4) x tipo record M (ORM)
003300010402     D*------------------
003400010402     D  DSBL4M       E DS
003500010402     D*------------------
003600030917      * DS FIAR5
003700030917     d dAr5Bnb       e ds
003800051219
003900051219     d Og143         e ds                  inz
004000060127
004100030917      *
004200021212     D Wtara           s                   like(§QTTPC)
004300021212     D Wntara          s                   like(TASPKF)
004400030917
004500030917     d kAr5Trd         s                   Like(Ar5Trd)
004600030515
004700030515     D Rma15           s             15
004800030515     D WRma15          s             15
004900030515     D II              s              2  0
005000030515     D Pos             s              2  0
005100111026     D Var             s              9
005200120510     D kaas            s                   like(tasaas) inz(2012)
005300010402
005400101025     C*------------------------------------------------------------------
005500120510     C                   EXSR      DEFCAM
005600120510      *
005700120510     c                   do        *hival
005800120510     c* leggo file conentente le spedizioni da elaborare
005900120510     c                   read      wurth_758
006000120510     c                   if        %eof(wurth_758)
006100120510     c                   leave
006200120510     c                   endif
006300120510     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
006400120510     c                   enddo
006500101025     c
006600921023     C                   SETON                                        LR
006700101025     C*----------------------------------------------------*
006800101025     c* operazioni  iniziali
006900101025     C*----------------------------------------------------*
007000910830     C     DEFCAM        BEGSR
007100050905      *
007200000721      *
007300030917      * FIAR
007400030917     c     kFiar5        Klist
007500030917     c                   Kfld                    TasAas
007600030917     c                   Kfld                    TasLnp
007700030917     c                   Kfld                    TasNrs
007800030917     c                   Kfld                    TasNsp
007900030917     c                   Kfld                    kAr5Trd
008000990817     C* TITAS
008100050905     C     KTAS          KLIST
008200120510     C                   KFLD                    kaas
008300120510     C                   KFLD                    lnp
008400120510     C                   KFLD                    nrs
008500120510     C                   KFLD                    nsp
008600990817     C* TITA7
008700050905     C     KTA7          KLIST
008800941122     C                   KFLD                    TASAAS
008900941122     C                   KFLD                    TASLNP
009000941122     C                   KFLD                    TASNRS
009100941122     C                   KFLD                    TASNSP
009200050905     C                   KFLD                    TASTBL
009300990817     C     *LIKE         DEFINE    TA4TRC        KA4TRC
009400990910     C     *LIKE         DEFINE    TASVA1        WVA
009500990910     C     *LIKE         DEFINE    TASSV1        WSV
009600050905     C     KTA4          KLIST
009700990817     C                   KFLD                    TASAAS
009800990817     C                   KFLD                    TASLNP
009900990817     C                   KFLD                    TASNRS
010000990817     C                   KFLD                    TASNSP
010100990817     C                   KFLD                    KA4TRC
010200010402     C*
010300050905     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
010400050905     C     KORM          KLIST
010500050905     C                   KFLD                    §B4POE
010600050905     C                   KFLD                    §B4NSR
010700050905     C                   KFLD                    §B4NOR
010800050905     C                   KFLD                    §B4NRV
010900990817     C*
011000990817     C* LEGGE IL RECORD SIGLE VARIE
011100990817     C     *LIKE         DEFINE    TBLKUT        KTBKUT
011200990817     C     *LIKE         DEFINE    TBLCOD        KTBCOD
011300990817     C     *LIKE         DEFINE    TBLKEY        KTBKEY
011400990817     C     KEYTAB        KLIST
011500990817     C                   KFLD                    KTBKUT
011600990817     C                   KFLD                    KTBCOD
011700990817     C                   KFLD                    KTBKEY
011800021212      *
011900021212      ** Aggancio tabella $2 - Varie forzate a programma
012000990817     C                   Z-ADD     1             KTBKUT
012100990817     C                   MOVEL     '$2'          KTBCOD
012200990817     C                   MOVEL     1             KTBKEY
012300990817     C     KEYTAB        CHAIN     TABEL00F                           99
012400990817IF  1C     *IN99         IFEQ      *OFF
012500990817     C     TBLFLG        ANDEQ     *BLANKS
012600990817     C                   MOVEL     TBLUNI        DS$2
012700990817E   1C                   ENDIF
012800021212      *
012900050905      ** Aggancio tabella QT - campi standard tassazione
013000021212     C                   MOVEL     'QT'          KTBCOD
013100021212     C                   MOVEL     1             KTBKEY
013200021212     C     KEYTAB        CHAIN     TABEL00F                           99
013300021212IF  1C     *IN99         IFEQ      *OFF
013400021212     C     TBLFLG        ANDEQ     *BLANKS
013500021212     C                   MOVEL     TBLUNI        DSQT1
013600021212E   1C                   ENDIF
013700990817     C*
013800910830     C                   ENDSR
013900050906     C*--------------------------------------------------------
014000050905     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
014100000124     C*--------------------------------------------------------
014200921023     C     WRTFIL        BEGSR
014300120510     C     KTAS          chain     TITAS30C
014400120510     c                   if        %found(titas30c) and tasnft>0
014500120510     c* tabella 4c
014600120510     C                   Z-ADD     1             KTBKUT
014700120510     C                   MOVEL     '4C'          KTBCOD
014800120510     C                   MOVEL(p)  tasksc        KTBKEY
014900120510     C                   clear                   ds4c
015000120510     c     keytab        chain     tabel00f
015100120510     c                   if        %found(tabel00f)
015200120510     c                   eval      ds4c=tbluni
015300120510     c                   endif
015400120510
015500101025     c                   exsr      SCriviDati
015600940910     C                   END
015700000124     C                   ENDSR
015800101025     C*-----------------------------------------------------------*
015900101025     C* Scrivo dati nel file TIVAF
016000101025     C*-----------------------------------------------------------*
016100101025     c     Scrividati    BEGSR
016200120510
016300101025     C* Inizializzo il buffer d output
016400120510     C                   clear                   wfvaf000
016500101025     C*
016600101025     C                   MOVEL     TASTBL        VAFTBL
016700101025     C                   Z-ADD     TASLNP        VAFLNP
016800101025     C                   Z-ADD     TASMGS        VAFMGS
016900101025     C                   Z-ADD     TASAAS        VAFAAS
017000101025     C                   Z-ADD     TASNRS        VAFNRS
017100101025     C                   Z-ADD     TASNSP        VAFNSP
017200101025     C                   Z-ADD     TASKSC        VAFKSC
017300101025     C                   MOVEL     TASFIN        VAFFIN
017400101025     C                   Z-ADD     TASLNA        VAFLNA
017500101025      * se particolità varia = 'O' recupero da FIAR5 il numero colli originali
017600101025      *
017700101025     c                   If        %Subst(TasGva:1:1) = 'O'
017800101025
017900101025     c                   eval      kAr5Trd  = 'BNB'
018000101025     c                   clear                   dAr5Bnb
018100101025     c     kFiar5        Chain     Fiar531c
018200101025     c                   If        %Found(Fiar531c)
018300101025     c                   Movel     Ar5Uni        dAr5Bnb
018400101025     c                   EndIf
018500101025      * Colli originali
018600101025     c                   Eval      VafNcl = §Ar5bcor
018700101025
018800101025     c                   else
018900101025      * Colli TITAS
019000101025     C                   Z-ADD     TASNCL        VAFNCL
019100101025     c                   endif
019200101025
019300101025     C                   Z-ADD     TASVLF        VAFVLF
019400101025     C                   Z-ADD     TASPOR        VAFPOR
019500101025      * PESO
019600101025     C                   Z-ADD     TASPKF        VAFPKB
019700101025     C                   IF        TASFPF = 'V'
019800101025     C     §QTTPC        MULT      TASNCP        WTARA
019900101025     C     TASPKC        SUB(h)    WTARA         WNTARA
020000101025    *C                   Z-ADD     WNTARA        VAFPKC
020100101025     C                   ELSE
020200101025    *C                   Z-ADD     *zeros        VAFPKC
020300101025     C                   ENDIF
020400101025     C* AZZERO CAMPI NON SEMPRE PRESENTI
020500101025     C                   MOVE      *ZERO         WSV
020600101025     C                   MOVE      *ZERO         WVA
020700101025     C                   MOVEA     *BLANKS       VSV
020800101025     C                   MOVEA     *ZERO         VVA
020900101025     C                   CLEAR                   A
021000101025     C                   CLEAR                   VAFSV1
021100101025     C                   CLEAR                   VAFVA1
021200101025     C                   CLEAR                   VAFSV2
021300101025     C                   CLEAR                   VAFVA2
021400101025     C                   CLEAR                   VAFSV3
021500101025     C                   CLEAR                   VAFVA3
021600101025     C                   CLEAR                   VAFSV4
021700101025     C                   CLEAR                   VAFVA4
021800101025     C                   CLEAR                   VAFSV5
021900101025     C                   CLEAR                   VAFVA5
022000101025     C                   CLEAR                   VAFSV6
022100101025     C                   CLEAR                   VAFVA6
022200101025     C                   CLEAR                   VAFSV7
022300101025     C                   CLEAR                   VAFVA7
022400101025     C                   CLEAR                   VAFSV8
022500101025     C                   CLEAR                   VAFVA8
022600101025     C                   CLEAR                   VAFVAX
022700101025     C*
022800101025     C                   MOVEL     TASSV1        WSV
022900101025     C                   Z-ADD     TASVA1        WVA
023000101025     C                   EXSR      CONTR
023100101025     C                   MOVEL     TASSV2        WSV
023200101025     C                   Z-ADD     TASVA2        WVA
023300101025     C                   EXSR      CONTR
023400101025     C                   MOVEL     TASSV3        WSV
023500101025     C                   Z-ADD     TASVA3        WVA
023600101025     C                   EXSR      CONTR
023700101025     C*
023800101025     C                   Z-ADD     TASDFT        VAFDFT
023900101025     C                   Z-ADD     TASNFT        VAFNFT
024000101025      *
024100101025     c                   if        tascbo <> 'FO'
024200101025     C                   Z-ADD     TASRMN        VAFRMN
024300101025     c                   endif
024400101025      *
024500101025     C                   MOVEL     TASFTC        VAFFTC
024600101025     C                   MOVEL     TASTSP        VAFTSP
024700101025     C                   MOVEL     TASCTS        VAFCTS
024800101025     C                   MOVEL     TASFAP        VAFFAP
024900101025     C                   Z-ADD     TASCTR        VAFCTR
025000101025     C                   Z-ADD     TASQFT        VAFQFT
025100101025     C                   MOVEL     TASTC2        VAFTC2
025200101025     C                   Z-ADD     TASIMV        VAFIMV
025300101025     C                   Z-ADD     TASFIV        VAFFIV
025400101025     C                   MOVEL     TASDIV        VAFDIV
025500101025     C                   Z-ADD     TASDRT        VAFDRT
025600160412     c*****!!!!! TASNAS non è più da utilizzare!!!! sostituire con §TA4ANAS
025700101025     C                   MOVEL     TASNAS        VAFNAS
025800160412     c*****!!!!! TASNAS non è più da utilizzare!!!! sostituire con §TA4ANAS
025900101025     C                   MOVEL     TASCAD        VAFCAD
026000101025     C                   MOVEL     TASLOD        VAFLOD
026100101025     C                   MOVEL     TASPRD        VAFPRD
026200101025     C                   MOVEL     TASNZD        VAFNZD
026300101025     C                   MOVEL     TASVAS        VAFVAS
026400101025     C                   Z-ADD     TASIAS        VAFIAS
026500101025    *C                   MOVEL     *blanks       VAFFPC
026600101025    *C                   MOVEL     *blanks       VAFFVC
026700101025    *C                   Z-ADD     *zeros        VAFVLC
026800101025     C     TASSV3        IFNE      *BLANK
026900101025     C                   EXSR      SRTA7
027000101025     C                   END
027100101025     C                   MOVEL     *BLANKS       VAFRMA
027200101025     C                   MOVEL     'A'           KA4TRC
027300101025     C     KTA4          CHAIN     TITA430C                           59
027400101025     C* se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
027500101025     c                   if        tascbo <> 'FO' and *in59 = *OFF
027600101025     C                   MOVEL     TA4NOT        VAFRMA
027700101025     C                   ENDIF
027800101025     C* se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
027900101025     c                   if        tascbo = 'FO' and *in59 = *OFF
028000101025     C                   MOVEL     TA4NOT        DTA4A
028100101025     c                   if        §ta4arma <> *blanks
028200101025     C                   MOVEL     §TA4ARMA      Rma15
028300101025      * verifico se campo numerico escludendo i blanks
028400101025     c                   eval      II = 1
028500101025     c                   dow       II <= %len(%trim(Rma15))
028600101025      *
028700101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
028800101025     c                   eval      WRma15 = (%trim(Wrma15)
028900101025     c                                      + %subst(%trim(Rma15):II:1))
029000101025     c                   endif
029100101025      *
029200101025     c                   eval      II = II + 1
029300101025      *
029400101025     c                   enddo
029500101025     c                   clear                   rma15
029600101025     c                   evalr     rma15 = %trim(Wrma15)
029700101025      *
029800101025     c                   eval      II = 1
029900101025     c                   setoff                                       29
030000101025     c                   dow       II <= %len(%trim(Rma15))
030100101025      *
030200101025     c                   If        %subst(%trim(Rma15):II:1) <> *blank
030300101025     c                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
030400101025     c                             VALNUM(1))
030500101025     c                   if        pos = 0
030600101025     c                   seton                                        29
030700101025     c                   leave
030800101025     c                   endif
030900101025     c                   endif
031000101025      *
031100101025     c                   eval      II = II + 1
031200101025      *
031300101025     c                   enddo
031400101025     c                   if        *in29 = *OFF
031500101025     c                   eval      VAFRMA= %editc(tasrmn:'Z')
031600101025     C                   MOVEL     rma15         VAFRMN
031700101025     c                   else
031800101025     C                   EVAL      VAFRMA = §ta4arma
031900101025     C                   EVAL      VAFRMN = tasrmn
032000101025     c                   endif
032100101025      *
032200101025     C                   endif
032300101025     C                   endif
032400101025     C*
032500101025     C* SCRIVO LE VARIE
032600101025     C                   Z-ADD     1             A
032700101025     C     VSV(A)        DOWNE     ' '
032800101025     C                   SELECT
032900101025     C     VAFSV1        WHENEQ    ' '
033000101025     C                   MOVEL     VSV(A)        VAFSV1
033100101025     C                   Z-ADD     VVA(A)        VAFVA1
033200101025     C*
033300101025     C     VAFSV2        WHENEQ    ' '
033400101025     C                   MOVEL     VSV(A)        VAFSV2
033500101025     C                   Z-ADD     VVA(A)        VAFVA2
033600101025     C*
033700101025     C     VAFSV3        WHENEQ    ' '
033800101025     C                   MOVEL     VSV(A)        VAFSV3
033900101025     C                   Z-ADD     VVA(A)        VAFVA3
034000101025     C*
034100101025     C     VAFSV4        WHENEQ    ' '
034200101025     C                   MOVEL     VSV(A)        VAFSV4
034300101025     C                   Z-ADD     VVA(A)        VAFVA4
034400101025     C*
034500101025     C     VAFSV5        WHENEQ    ' '
034600101025     C                   MOVEL     VSV(A)        VAFSV5
034700101025     C                   Z-ADD     VVA(A)        VAFVA5
034800101025     C*
034900101025     C     VAFSV6        WHENEQ    ' '
035000101025     C                   MOVEL     VSV(A)        VAFSV6
035100101025     C                   Z-ADD     VVA(A)        VAFVA6
035200101025     C*
035300101025     C     VAFSV7        WHENEQ    ' '
035400101025     C                   MOVEL     VSV(A)        VAFSV7
035500101025     C                   Z-ADD     VVA(A)        VAFVA7
035600101025     C*
035700101025     C     VAFSV8        WHENEQ    ' '
035800101025     C                   MOVEL     VSV(A)        VAFSV8
035900101025     C                   Z-ADD     VVA(A)        VAFVA8
036000101025     C                   OTHER
036100101025     C                   ADD       VVA(A)        VAFVAX
036200101025     C                   ENDSL
036300101025     C*
036400101025     C                   ADD       1             A
036500101025     C                   ENDDO
036600101025      * Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
036700101025     c                   Clear                   VafRmx
036800101025     C                   MOVEL     VAFKSC        WLB3              3 0
036900101025     c     WLB3          chain     AZORG
037000101025     c                   if            %found(AZORG01L)
037100101025     c                             and ORGfva = *blanks
037200101025     c                   movel     ORGde3        Og143
037300101025     c                   else
037400101025     c                   clear                   Og143
037500101025     c                   endif
037600101025
037700101025     c                   Select
037800101025
037900101025     C* Recupera Riferimento Alfanumerico da ORM
038000101025     C                   When      §4cRfo = 'S'
038100101025     C                   MOVEL     'M'           KA4TRC
038200101025     C     KTA4          CHAIN     TITA430C                           59
038300101025     C     *IN59         IFEQ      *OFF
038400101025     C                   MOVEL(P)  TA4NOT        DSBL4M
038500101025     C     KORM          CHAIN     FNORM01L                           61
038600101025     C     *IN61         IFEQ      *OFF
038700101025     C                   MOVEL     ORMRFA        VAFRMX
038800101025     C                   ENDIF
038900101025     C                   ENDIF
039000101025
039100101025     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
039200101025     c                   When      §OGntw = 'DPD'
039300101025     C                   movel     TASDCM        VAFRMX
039400101025
039500101025     c                   EndSl
039600120510     c* prelevo cup / cig
039700120510     C                   MOVEL     '4'           KA4TRC
039800120510     C     KTA4          CHAIN     TITA430C                           59
039900120510     c                   if        %found(tita430c)
040000120510     c                   movel     ta4not        dta44
040100120510     c                   eval      vafcup=§TA4CUP
040200120510     c                   eval      vafcig=§TA4Cig
040300120510     c                   endif
040400120510
040500120510     c                   write     wfvaf000
040600101025     c
040700101025     c                   ENDSR
040800000124     C*-----------------------------------------------------------*
040900000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
041000000124     C*-----------------------------------------------------------*
041100941122     C     SRTA7         BEGSR
041200990817     C*
041300990823     C     KTA7          SETLL     TITA730C
041400990823     C     KTA7          READE     TITA730C                               59
041500990817DO  1C     *IN59         DOWEQ     *OFF
041600990910     C                   MOVEL     TA7SVN        WSV
041700990910     C                   Z-ADD     TA7VAN        WVA
041800990910     C                   EXSR      CONTR
041900990910     C*
042000990823     C     KTA7          READE     TITA730C                               59
042100990817E   1C                   ENDDO
042200990817     C*
042300941122     C                   ENDSR
042400990910     C*--------------------------------------------------------
042500000124     C* CONTROLLO SE VARIE GIA' PRESENTI
042600000124     C*--------------------------------------------------------
042700990910     C     CONTR         BEGSR
042800000124IF  1C     WSV           IFNE      ' '
042900990910     C*
043000990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
043100990910     C     WSV           ANDNE     §$2IVA
043200990910     C                   ADD       1             A                 3 0
043300990910     C                   MOVEL     WSV           VSV(A)
043400990910     C                   Z-ADD     WVA           VVA(A)
043500990910E   4C                   ENDIF
043600000124E   1C                   ENDIF
043700000124     C                   ENDSR
043800050905
043900990817     C*--------------------------------------------------------
044000030515**         VALNUM
0441000305150123456789
