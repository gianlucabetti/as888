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
001400150917
001500890921     D KPJBA         E DS
001600990817     D DS4C          E DS
001700050906     D DSQT1         E DS
001800050906     D DS$2          E DS
001900030516     D DTA4A         E DS
002000160209     D DTA4M         E DS
002100151016     D DAR5FAT       E DS
002200050905     D tivafds       e ds                  extname(tivaf00t)
002300150922
002400150922
002500150922     D* Strutture per passaggio parametri a driver per calcolo tassazione
002600150922     D IN_PF_OPZ       S              1    INZ(*blanks)
002700160530     D IN_PF_DSTAS   E DS                  EXTNAME(titas00f)
002800160530     D OUT_PF_DTAS   E DS                  EXTNAME(DTAS)    PREFIX(PF_)
002900160530     D OUT_PF_DTASV  E DS                  EXTNAME(DTASV)   PREFIX(PF_)
003000160503     D  TSV                    1     20                                         (T = da TNSF22R)
003100150922     D                                     DIM(20)                              Sigle varie
003200160503     D  TVA                   21    140P 3                                      (T = da TNSF22R)
003300150922     D                                     DIM(20)                              Importi varie
003400160530     D OUT_PF_DTASPESE DS                  EXTNAME(DTASPES) PREFIX(PF_)
003500150922
003600150922
003700090423     D PARAM           DS
003800090423     D  PARDATADA                     8S 0
003900090423     D  PARDATAA                      8S 0
004000090424     D  PARCLI                        7S 0
004100090424     D  PARTIPCLI                     1A
004200090423     D  PARCLIVGD                     8A
004300090424     D  PARTIPFIL                     2A
004400090424     D  PARGGBACK                     3S 0
004500090424     D  PARGGDAELAB                   3S 0
004600150922     D  PARTYPE                       1A                                        A=typf AF P=typf PF
004700150921     D  PARFORZAFATT                  1A
004800150922
004900150922     D* DS FIAR5
005000150922     D dAr5Bnb       e ds
005100051219
005200150922     D Og143         e ds                  inz
005300060127     D trul47ds      e ds
005400150922     D*
005500021212     D Wtara           s                   like(§QTTPC)
005600021212     D Wntara          s                   like(TASPKF)
005700030917
005800150922     D kAr5Trd         s                   Like(Ar5Trd)
005900030515
006000030515     D Rma15           s             15
006100030515     D WRma15          s             15
006200030515     D II              s              2  0
006300030515     D Pos             s              2  0
006400160209
006500160209     D*--------------------
006600160209     D* DEFINIZIONI A PROCEDURE ESTERNE
006700160209     D*--------------------
006800160209     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
006900160209
007000010402
007100010402
007200060127     C*
007300090424     C* Come prima cosa avvio il blocco elaborazione TIVGD x tipo file corrente
007400060127     C                   clear                   trul47ds
007500060127     C                   eval      d47opz  = 'I'
007600090424     C                   eval      d47tip  = PARTIPFIL
007700060127     C                   eval      d47lck  = 'N'
007800060127     C                   eval      d47chkj = 'N'
007900150921     C                   eval      d47pgm  = 'TRTC28R3'
008000060127     C                   call      'TRUL47R'
008100060127     C                   parm                    trul47ds
008200060127     C*
008300060127     C* Se elaborazione consentita => proseguo
008400060127     C                   if        d47sts <> 'A'
008500060127     C*
008600921023     C                   EXSR      DEFCAM
008700090424      *
008800090424      * Gestisco se richiesto cliente SINGOLO o UNIFICANTE
008900090424     C                   if        PARTIPCLI = 'U'
009000040806      *
009100921023     C     KTB           SETLL     TABEL
009200040806    1C                   DO        *HIVAL
009300921023     C     KTB           READE     TABEL                                  58
009400040806    2C  N58              DO
009500040806    3C     TBLFLG        IFNE      '*'
009600921023     C                   MOVEL     TBLUNI        DS4C
009700040806    4C     §4CFSD        IFNE      *BLANK                                        CLI DA SCAR
009800050906     C                   MOVE      §4CFKS        KSCU              7 0
009900050906     C                   MOVEL     TBLKEY        KSC               7 0
010000981203     C*
010100090424    5C     PARCLI        IFGT      *ZERO                                         PARZIALIZZ.
010200090424     C     KSCU          ANDEQ     PARCLI                                        CLIENTE
010300000721     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
010400090424   e5C                   ENDIF
010500090424   e4C                   ENDIF
010600090424   e3C                   ENDIF
010700090424   e2C                   ENDDO
010800090424   e1C  N58              ENDDO
010900060127     C*
011000090424     C                   else
011100090424     C                   eval      KSC = PARCLI
011200090424     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
011300090424     C                   endif
011400090424     C*
011500060127     C                   endif
011600060127     C*
011700060127     C* Infine elimino il blocco elaborazione TIVGD x tipo file corrente: 'BT'
011800060127     C                   clear                   trul47ds
011900060127     C                   eval      d47opz  = 'F'
012000090424     C                   eval      d47tip  = PARTIPFIL
012100060127     C                   call      'TRUL47R'
012200060127     C                   parm                    trul47ds
012300160209     C*
012400160209     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
012500160209     C                   CALL      'UBTA400R'
012600160209     C                   PARM                    UBTA4IOPZ
012700160209     C                   PARM      'C'           UBTA4ITLA
012800160209     C                   PARM                    UBTA4IAAS
012900160209     C                   PARM                    UBTA4ILNP
013000160209     C                   PARM                    UBTA4INRS
013100160209     C                   PARM                    UBTA4INSP
013200160209     C                   PARM                    UBTA4ITRC
013300160209     C                   PARM                    UBTA4OERR
013400160209     C                   PARM                    UBTA4ODS
013500160209     C                   PARM                    UBTA4OLEN
013600160209     C                   PARM                    UBTA4ODATI
013700991110     C*
013800921023     C                   SETON                                        LR
013900910830     C*----------------------------------------------------*
014000910830     C     DEFCAM        BEGSR
014100021212      *
014200021212      ** Aggancio tabella $2 - Varie forzate a programma
014300990817     C                   Z-ADD     1             KTBKUT
014400990817     C                   MOVEL     '$2'          KTBCOD
014500990817     C                   MOVEL     1             KTBKEY
014600990817     C     KEYTAB        CHAIN     TABEL00F                           99
014700990817IF  1C     *IN99         IFEQ      *OFF
014800990817     C     TBLFLG        ANDEQ     *BLANKS
014900990817     C                   MOVEL     TBLUNI        DS$2
015000990817E   1C                   ENDIF
015100021212      *
015200050905      ** Aggancio tabella QT - campi standard tassazione
015300021212     C                   MOVEL     'QT'          KTBCOD
015400021212     C                   MOVEL     1             KTBKEY
015500021212     C     KEYTAB        CHAIN     TABEL00F                           99
015600021212IF  1C     *IN99         IFEQ      *OFF
015700021212     C     TBLFLG        ANDEQ     *BLANKS
015800021212     C                   MOVEL     TBLUNI        DSQT1
015900021212E   1C                   ENDIF
016000990817     C*
016100021212      ** Imposto key per tabella clienti con ritorno info fattura
016200990817     C                   MOVE      1             TBLKUT
016300990817     C                   MOVEL     '4C'          TBLCOD
016400990817     C                   MOVEL     *BLANKS       TBLKEY
016500990817     C*
016600910830     C                   ENDSR
016700050906     C*--------------------------------------------------------
016800090423     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
016900000124     C*--------------------------------------------------------
017000921023     C     WRTFIL        BEGSR
017100090423     C*
017200090424     C* Mi posiziono x cliente
017300090423     C                   setoff                                       70
017400090423     C*
017500090424     C     KTAS          SETLL     TITAS31C
017600090424     C                   IF        %found(titas31C)
017700090424     C                   READ      titas31c
017800090424     C                   DOW       not %eof(titas31c) and not *in70
017900090423     C*
018000150917     C* Verifico se il cliente è quello richiesto
018100090424     C                   if        tasKSC <> KSC
018200090423     C                   seton                                        70
018300090423     C                   else
018400150917     C*
018500150917     C* Verifico se data spedizione è nel periodo richiesto
018600090424     C                   if        tasAAS*10000+tasMGS >= wDataDa AND
018700090424     C                             tasAAS*10000+tasMGS <= wDataA
018800150922     C*
018900150922     C* Gestisco se nei parametri in input:
019000150922     C*    - se richiesto lancio come A = ANTEPRIMA  FATTURE (AF)
019100150922     C*    - se richiesto lancio come P = PREVISIONE FATTURE (PF)
019200150922     C*    - il default è                 ANTEPRIMA  FATTURE (AF)
019300150922     C*
019400150922     C                   SELECT
019500150922     C                   WHEN      PARTYPE = 'A'
019600150922     C                   MOVEL     '1'           wInvoiced
019700150922     C*
019800150922     C                   WHEN      PARTYPE = 'P'
019900150917     C*
020000150922     C* Inizializzo il flag che indica se spedizione già fatturata o meno
020100150921     C                   MOVEL     '0'           wInvoiced         1
020200150917     C                   IF        TASDFT > *zeros OR TASNFT > *zeros
020300150917     C                   MOVEL     '1'           wInvoiced
020400150917     C                   ENDIF
020500150921     C*
020600150921     C* Se richiesto nei parametri forzo lo stato di "spedizione già fatturata o meno"
020700150921     C                   IF        PARFORZAFATT <> *blanks
020800150921     C                   SELECT
020900150921     C                   WHEN      PARFORZAFATT = 'S'
021000150921     C                   MOVEL     '1'           wInvoiced
021100150921     C                   WHEN      PARFORZAFATT = 'N'
021200150921     C                   MOVEL     '0'           wInvoiced
021300150921     C                   ENDSL
021400150921     C                   ENDIF
021500150922     C*
021600150922     C                   OTHER
021700150922     C                   MOVEL     '1'           wInvoiced
021800150922     C                   ENDSL
021900150921     C*
022000150921     C* Inizializzazioni varie
022100150921     C                   MOVE      *ZERO         WSV
022200150921     C                   MOVE      *ZERO         WVA
022300150921     C                   MOVEA     *BLANKS       VSV
022400150921     C                   MOVEA     *ZERO         VVA
022500150921     C                   CLEAR                   A
022600150921     C                   CLEAR                   VAFSV1
022700150921     C                   CLEAR                   VAFVA1
022800150921     C                   CLEAR                   VAFSV2
022900150921     C                   CLEAR                   VAFVA2
023000150921     C                   CLEAR                   VAFSV3
023100150921     C                   CLEAR                   VAFVA3
023200150921     C                   CLEAR                   VAFSV4
023300150921     C                   CLEAR                   VAFVA4
023400150921     C                   CLEAR                   VAFSV5
023500150921     C                   CLEAR                   VAFVA5
023600150921     C                   CLEAR                   VAFSV6
023700150921     C                   CLEAR                   VAFVA6
023800150921     C                   CLEAR                   VAFSV7
023900150921     C                   CLEAR                   VAFVA7
024000150921     C                   CLEAR                   VAFSV8
024100150921     C                   CLEAR                   VAFVA8
024200150921     C                   CLEAR                   VAFVAX
024300150917     C*
024400150917     C* ***********
024500150917     C* Se spedizione non ancora fatturata =>
024600150917     C* ***********
024700150922     C                   if        wInvoiced = *off
024800150922     C*
024900150922     C                   call      'UBPREFATR'
025000150922     C                   parm                    IN_PF_OPZ
025100150922     C                   parm                    IN_PF_DSTAS
025200150922     C                   parm                    OUT_PF_DTAS
025300150922     C                   parm                    OUT_PF_DTASV
025400151020     C                   parm                    OUT_PF_DTASPES
025500150922     C
025600150922     C                   exsr      VALDTAS
025700150921     C*
025800150922     C                   endif
025900160530     C*
026000160530     C* ***********
026100160530     C* Reperisco tutti i dati necessari non presenti in forma diretta su TITAS
026200160530     C* ***********
026300160530     C*
026400160530     C* Se particolità varia = 'O' recupero da FIAR5 il numero colli originali
026500160530     C                   If        %Subst(TASGVA:1:1) = 'O'
026600160530     C                   eval      kAr5Trd  = 'BNB'
026700160530     C                   clear                   dAr5Bnb
026800160530     C     kFiar5        Chain     Fiar531c
026900160530     C                   If        %Found(Fiar531c)
027000160530     C                   Movel     Ar5Uni        dAr5Bnb
027100160530     C                   EndIf
027200160530     C                   Eval      TASNCL = §Ar5bcor                            Colli originali
027300160530     C                   endif
027400050905     C*
027500050905     C* Inizializzo il buffer d output
027600050905     C                   clear                   TIVAFDS
027700160209     C*
027800160209     C* Reperisco TITA4 tipo record 'A'
027900160209     C                   MOVEL     'A'           UBTA4ITRC
028000160209     C                   EXSR      RTVTA4
028100150917     C*
028200150917     C                   Z-ADD     TASNCL        VAFNCL
028300050905     C*
028400941122     C                   MOVEL     TASTBL        VAFTBL
028500941122     C                   Z-ADD     TASLNP        VAFLNP
028600941122     C                   Z-ADD     TASMGS        VAFMGS
028700941122     C                   Z-ADD     TASAAS        VAFAAS
028800941122     C                   Z-ADD     TASNRS        VAFNRS
028900941122     C                   Z-ADD     TASNSP        VAFNSP
029000941122     C                   Z-ADD     TASKSC        VAFKSC
029100941122     C                   MOVEL     TASFIN        VAFFIN
029200941122     C                   Z-ADD     TASLNA        VAFLNA
029300941122     C                   Z-ADD     TASVLF        VAFVLF
029400941122     C                   Z-ADD     TASPOR        VAFPOR
029500150921     C* PESO
029600021211     C                   Z-ADD     TASPKF        VAFPKB
029700021211     C                   IF        TASFPF = 'V'
029800150716     C*
029900150716     C                   IF        TASDFT <= §QTDST
030000150716     C     §QTTCO        MULT      TASNCP        WTARA
030100150716     C     TASPKC        SUB(h)    WTARA         WNTARA
030200150716     C                   Z-ADD     WNTARA        VAFPKC
030300150716     C                   ELSE
030400150716     C     §QTTPC        MULT      TASNCP        WTARA
030500150716     C     TASPKC        SUB(h)    WTARA         WNTARA
030600150716     C                   Z-ADD     WNTARA        VAFPKC
030700150716     C                   ENDIF
030800150716     C*
030900021211     C                   ELSE
031000151020     C                   IF        TASFPF <> 'D'
031100150716     C                   Z-ADD     *zeros        VAFPKC
031200151020     C                   ENDIF
031300021211     C                   ENDIF
031400000124     C*
031500150922     C* Se trattasi di spedizione già fatturata
031600150922     C                   IF        wInvoiced = *on
031700150917     C*
031800150917     C                   MOVEL     TASSV1        WSV
031900990910     C                   Z-ADD     TASVA1        WVA
032000990910     C                   EXSR      CONTR
032100990910     C                   MOVEL     TASSV2        WSV
032200990910     C                   Z-ADD     TASVA2        WVA
032300990910     C                   EXSR      CONTR
032400990910     C                   MOVEL     TASSV3        WSV
032500990910     C                   Z-ADD     TASVA3        WVA
032600990910     C                   EXSR      CONTR
032700150917     C*
032800150917     C     TASSV3        IFNE      *BLANK
032900150917     C                   EXSR      SRTA7
033000150917     C                   ENDIF
033100990817     C*
033200941122     C                   Z-ADD     TASDFT        VAFDFT
033300941122     C                   Z-ADD     TASNFT        VAFNFT
033400150917     C*
033500150922     C* Se trattasi di spedizione NON già fatturata
033600150921     C                   ELSE
033700150921     C                   EXSR      TVA2CONTR
033800150921     C*
033900150922     C                   ENDIF
034000150917     C*
034100150917     C                   if        tascbo <> 'FO'
034200941122     C                   Z-ADD     TASRMN        VAFRMN
034300150917     C                   endif
034400150917     C*
034500941122     C                   MOVEL     TASFTC        VAFFTC
034600941122     C                   MOVEL     TASTSP        VAFTSP
034700941122     C                   MOVEL     TASCTS        VAFCTS
034800941122     C                   MOVEL     TASFAP        VAFFAP
034900941122     C                   Z-ADD     TASCTR        VAFCTR
035000941122     C                   Z-ADD     TASQFT        VAFQFT
035100941122     C                   MOVEL     TASTC2        VAFTC2
035200000124     C                   Z-ADD     TASIMV        VAFIMV
035300000124     C                   Z-ADD     TASFIV        VAFFIV
035400000124     C                   MOVEL     TASDIV        VAFDIV
035500000124     C                   Z-ADD     TASDRT        VAFDRT
035600160209     C                   MOVEL     §TA4ANAS      VAFNAS
035700000124     C                   MOVEL     TASCAD        VAFCAD
035800000124     C                   MOVEL     TASLOD        VAFLOD
035900000124     C                   MOVEL     TASPRD        VAFPRD
036000000124     C                   MOVEL     TASNZD        VAFNZD
036100000124     C                   MOVEL     TASVAS        VAFVAS
036200000124     C                   Z-ADD     TASIAS        VAFIAS
036300000124    *C                   MOVEL     *blanks       VAFFPC
036400000124    *C                   MOVEL     *blanks       VAFFVC
036500000124    *C                   Z-ADD     *zeros        VAFVLC
036600030515     C                   MOVEL     *BLANKS       VAFRMA
036700150921     C* Se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
036800160222     C                   if        tascbo <> 'FO'
036900160209     C                   MOVEL     DTA4A         VAFRMA
037000000124     C                   ENDIF
037100150921     C* Se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
037200160222     C                   if        tascbo = 'FO'
037300150921     C                   if        §ta4arma <> *blanks
037400150921     C                   MOVEL     §TA4ARMA      Rma15
037500150921     C* Verifico se campo numerico escludendo i blanks
037600150921     C                   eval      II = 1
037700150921     C                   dow       II <= %len(%trim(Rma15))
037800150921     C*
037900150921     C                   If        %subst(%trim(Rma15):II:1) <> *blank
038000150921     C                   eval      WRma15 = (%trim(Wrma15)
038100150921     C                                      + %subst(%trim(Rma15):II:1))
038200150921     C                   endif
038300150921     C*
038400150921     C                   eval      II = II + 1
038500150921     C*
038600150921     C                   enddo
038700150921     C                   clear                   rma15
038800150921     C                   evalr     rma15 = %trim(Wrma15)
038900150921     C*
039000150921     C                   eval      II = 1
039100150921     C                   setoff                                       29
039200150921     C                   dow       II <= %len(%trim(Rma15))
039300150921     C*
039400150921     C                   If        %subst(%trim(Rma15):II:1) <> *blank
039500150921     C                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
039600150921     C                             VALNUM(1))
039700150921     C                   if        pos = 0
039800150921     C                   seton                                        29
039900150921     C                   leave
040000150921     C                   endif
040100150921     C                   endif
040200150921     C*
040300150921     C                   eval      II = II + 1
040400150921     C*
040500150921     C                   enddo
040600150921     C                   if        *in29 = *OFF
040700150921     C                   eval      VAFRMA= %editc(tasrmn:'Z')
040800150921     C                   MOVEL     rma15         VAFRMN
040900150921     C                   else
041000150921     C                   EVAL      VAFRMA = §ta4arma
041100150921     C                   EVAL      VAFRMN = tasrmn
041200150921     C                   endif
041300150921     C*
041400150921     C                   endif
041500030515     C                   endif
041600990910     C*
041700990910     C* SCRIVO LE VARIE
041800990910     C                   Z-ADD     1             A
041900990910     C     VSV(A)        DOWNE     ' '
042000990910     C                   SELECT
042100990910     C     VAFSV1        WHENEQ    ' '
042200990910     C                   MOVEL     VSV(A)        VAFSV1
042300990910     C                   Z-ADD     VVA(A)        VAFVA1
042400990910     C*
042500990910     C     VAFSV2        WHENEQ    ' '
042600990910     C                   MOVEL     VSV(A)        VAFSV2
042700990910     C                   Z-ADD     VVA(A)        VAFVA2
042800990910     C*
042900990910     C     VAFSV3        WHENEQ    ' '
043000990910     C                   MOVEL     VSV(A)        VAFSV3
043100990910     C                   Z-ADD     VVA(A)        VAFVA3
043200000124     C*
043300000124     C     VAFSV4        WHENEQ    ' '
043400000124     C                   MOVEL     VSV(A)        VAFSV4
043500000124     C                   Z-ADD     VVA(A)        VAFVA4
043600000124     C*
043700000124     C     VAFSV5        WHENEQ    ' '
043800000124     C                   MOVEL     VSV(A)        VAFSV5
043900000124     C                   Z-ADD     VVA(A)        VAFVA5
044000000124     C*
044100000124     C     VAFSV6        WHENEQ    ' '
044200000124     C                   MOVEL     VSV(A)        VAFSV6
044300000124     C                   Z-ADD     VVA(A)        VAFVA6
044400000124     C*
044500000124     C     VAFSV7        WHENEQ    ' '
044600000124     C                   MOVEL     VSV(A)        VAFSV7
044700000124     C                   Z-ADD     VVA(A)        VAFVA7
044800000124     C*
044900000124     C     VAFSV8        WHENEQ    ' '
045000000124     C                   MOVEL     VSV(A)        VAFSV8
045100000124     C                   Z-ADD     VVA(A)        VAFVA8
045200990910     C                   OTHER
045300990910     C                   ADD       VVA(A)        VAFVAX
045400990910     C                   ENDSL
045500990910     C*
045600990910     C                   ADD       1             A
045700150921     C                   ENDDO
045800150921     C* Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
045900150921     C                   Clear                   VafRmx
046000150921     C                   MOVEL     VAFKSC        WLB3              3 0
046100150921     C     WLB3          chain     AZORG
046200150921     C                   if            %found(AZORG01L)
046300150921     C                             and ORGfva = *blanks
046400150921     C                   movel     ORGde3        Og143
046500150921     C                   else
046600150921     C                   clear                   Og143
046700150921     C                   endif
046800150921     C*
046900150921     C                   Select
047000150921     C*
047100150921     C* Recupera Riferimento Alfanumerico da ORM
047200050126     C                   When      §4cRfo = 'S'
047300160209     C*
047400160209     C* Reperisco TITA4 tipo record 'M'
047500160209     C                   MOVEL     'M'           UBTA4ITRC
047600160209     C                   EXSR      RTVTA4
047700160209     C*
047800160209     C                   IF        DTA4M <> *zeros
047900050126     C     KORM          CHAIN     FNORM01L                           61
048000050126     C     *IN61         IFEQ      *OFF
048100050126     C                   MOVEL     ORMRFA        VAFRMX
048200050126     C                   ENDIF
048300050126     C                   ENDIF
048400160209     C*
048500150921     C* Ritorna al cliente il campo XCO
048600150921     C***                When      §4cRfo = 'X' and TasXco <> *Blanks
048700150921     C* recupero il valore da tornare dalla tabella VUS
048800150921     C***                Clear                   Tibs02ds
048900150921     C***                Clear                   dvus
049000150921     C***                Eval      T02mod = 'C'
049100150921     C***                Eval      T02sif = knsif
049200150921     C***                Eval      T02cod = 'VUS'
049300150921     C***                Eval      T02ke1 = TasXco
049400150921     C***                Call      'TIBS02R'
049500150921     C***                Parm                    kpjba
049600150921     C***                Parm                    Tibs02ds
049700150921     C***                If        T02err = *blanks
049800150921     C***                Eval      dvus = T02uni
049900150921     C***                EndIf
050000150921     C***                Eval      vafrmx = §vuvaf
050100150921     C*
050200150921     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
050300150921     C                   When      §OGntw = 'DPD'
050400150921     C                   movel     TASDCM        VAFRMX
050500150921     C*
050600150921     C                   EndSl
050700151016     C*
050800151016     C* Eseguo eventuali "forzature"
050900151016     C                   EXSR      RTVAR5FAT
051000151020     C                   EXSR      CHKPESDES
051100060203     C*
051200060203     C* Quindi scarico il buffer nel file d out generico download
051300050905     C                   exsr      WRIVGD
051400050905     C*
051500090423     C                   endif
051600090424     C                   endif
051700090423     C*
051800090423     C* Proseguo la lettura delle bolle
051900090424     C                   read      titas31c
052000090423     C*
052100090423     C                   ENDDO
052200090423     C                   ENDIF
052300090423     C*
052400000124     C                   ENDSR
052500150922     C*---------------------------------------------------------------*
052600150922     C*  VALORIZZAZIOEN CAMPI DS DTAS DA CAMPI TITAS IN INPUT
052700150922     C*---------------------------------------------------------------*
052800150922     C     VALDTAS       BEGSR
052900150922     C*
053000150922     C                   eval      TASKSC  = PF_TASKSC
053100150922     C                   eval      TASPKF  = PF_TASPKF
053200150922     C                   eval      TASPKC  = PF_TASPKC
053300150922     C                   eval      TASNCP  = PF_TASNCP
053400150922     C                   eval      TASVLF  = PF_TASVLF
053500150922     C                   eval      TASNCL  = PF_TASNCL
053600150922     C                   eval      TASCTR  = PF_TASCTR
053700150922     C                   eval      TASCTS  = PF_TASCTS
053800150922     C                   eval      TASNZD  = PF_TASNZD
053900150922     C                   eval      TASCAD  = PF_TASCAD
054000150922     C                   eval      TASFIN  = PF_TASFIN
054100150922     C                   eval      TASFDN  = PF_TASFDN
054200150922     C                   eval      TASMCT  = PF_TASMCT
054300150922     C                   eval      TASNZM  = PF_TASNZM
054400150922     C                   eval      TASCAM  = PF_TASCAM
054500150922     C                   eval      TASFAP  = PF_TASFAP
054600150922     C                   eval      TASTSP  = PF_TASTSP
054700150922     C                   eval      TASTC2  = PF_TASTC2
054800150922     C                   eval      TASLNP  = PF_TASLNP
054900150922     C                   eval      TASLNA  = PF_TASLNA
055000150922     C                   eval      TASTBL  = PF_TASTBL
055100150922     C                   eval      TASVAS  = PF_TASVAS
055200150922     C                   eval      TASIAS  = PF_TASIAS
055300150922     C                   eval      TASQFT  = PF_TASQFT
055400150922     C                   eval      TASDIV  = PF_TASDIV
055500150922     C                   eval      TASPOR  = PF_TASPOR
055600150922     C                   eval      TASIMV  = PF_TASIMV
055700150922     C                   eval      TASPVL  = PF_TASPVL
055800150922     C*
055900150922     C                   ENDSR
056000000124     C*-----------------------------------------------------------*
056100000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
056200000124     C*-----------------------------------------------------------*
056300941122     C     SRTA7         BEGSR
056400990817     C*
056500990823     C     KTA7          SETLL     TITA730C
056600990823     C     KTA7          READE     TITA730C                               59
056700990817DO  1C     *IN59         DOWEQ     *OFF
056800990910     C                   MOVEL     TA7SVN        WSV
056900990910     C                   Z-ADD     TA7VAN        WVA
057000990910     C                   EXSR      CONTR
057100990910     C*
057200990823     C     KTA7          READE     TITA730C                               59
057300990817E   1C                   ENDDO
057400990817     C*
057500941122     C                   ENDSR
057600990910     C*--------------------------------------------------------
057700000124     C* CONTROLLO SE VARIE GIA' PRESENTI
057800000124     C*--------------------------------------------------------
057900990910     C     CONTR         BEGSR
058000000124IF  1C     WSV           IFNE      ' '
058100990910     C*
058200990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
058300990910     C     WSV           ANDNE     §$2IVA
058400990910     C                   ADD       1             A                 3 0
058500990910     C                   MOVEL     WSV           VSV(A)
058600990910     C                   Z-ADD     WVA           VVA(A)
058700990910E   4C                   ENDIF
058800000124E   1C                   ENDIF
058900000124     C                   ENDSR
059000150921     C*--------------------------------------------------------
059100150921     C* CARICAMENTO "CONTR" DA PREVISIONE TASSAZIONE
059200150921     C*--------------------------------------------------------
059300150921     C     TVA2CONTR     BEGSR
059400150921     C*
059500150921     C                   DOW       A < %elem(TSV)
059600150921     C                   ADD       1             A                 3 0
059700150921     C                   MOVEL     TSV(A)        VSV(A)
059800150921     C                   Z-ADD     TVA(A)        VVA(A)
059900150921     C                   ENDDO
060000150921     C*
060100150921     C                   ENDSR
060200090423
060300090423
060400050905
060500050905      /TITLE Scrittura record TIVAF in file TIVGD00F (file VAS generico download)
060600050905     C     wriVGD        BEGSR
060700050905     C*
060800050905     C                   clear                   tivgd000
060900090423     C                   eval      vgdDTA = TIVAFDS
061000090424     C                   eval      vgdTIP = PARTIPFIL
061100090423     C                   eval      vgdKSU = PARCLIVGD
061200090423     C                   eval      vgdTSC = 'WW'
061300050905     C                   eval      vgdDAT = datcor
061400150921     C                   eval      vgdPGM = 'TRTC28R3'
061500050905     C                   write     tivgd000
061600050905     C*
061700050905     C                   ENDSR
061800151016
061900151016
062000151016
062100151020     C*--------------------------------------------------------
062200151016     C     RTVAR5FAT     BEGSR
062300151016     C*
062400151020     C* Se, presente, considero il peso/volume DESUNTO (già fatturato) al pari del VDL TOTALE
062500151028     C                   SETOFF                                       7172
062600151016     C                   EVAL      kAr5Trd = 'FAT'
062700151016     C                   CLEAR                   DAR5FAT
062800151016     C     kFiar5        CHAIN     FIAR531C
062900151016     C                   IF        %found(FIAR531C)
063000151016     C                   MOVEL     AR5UNI        DAR5FAT
063100161214     C                   IF        §AR5FPTAS = 'D' AND §AR5PKTAS > *zeros
063200151028     C                   SETON                                        71
063300151016     C                   EVAL      VAFPKC = §AR5PKTAS
063400151016     C                   ENDIF
063500161214     C                   IF        §AR5FVTAS = 'D' AND §AR5VLTAS > *zeros
063600151028     C                   SETON                                        72
063700160530     C                   EVAL      VAFVLC = §AR5VLTAS
063800151016     C                   ENDIF
063900151016     C                   ENDIF
064000151016     C*
064100151016     C                   ENDSR
064200151020     C*--------------------------------------------------------
064300151020
064400151020
064500151020
064600151020     C*--------------------------------------------------------
064700151020     C     CHKPESDES     BEGSR
064800151020     C*
064900151028     C                   SETOFF                                       73
065000151020     C*
065100151020     C* Se, presente, considero il peso/volume DESUNTO (calcolato run-time) al pari del VDL TOTALE
065200151020     C                   IF        PF_taspDES = 'S'
065300151020     C                   IF        PF_taspPKD > *zeros
065400151028     C                   SETON                                        73
065500151020     C                   EVAL      VAFPKC = PF_taspPKD
065600151020     C                   ENDIF
065700151020     C                   ENDIF
065800151020     C*
065900151020     C                   ENDSR
066000151020     C*--------------------------------------------------------
066100160209
066200160209
066300160209
066400160209     C     RTVTA4        BEGSR
066500160209     C*
066600160209     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
066700160209     C                   CLEAR                   DTA4A
066800160209     C                   CLEAR                   DTA4M
066900160209     C*
067000160209     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
067100160209     C                   CALL      'UBTA400R'
067200160209     C                   PARM      *blanks       UBTA4IOPZ
067300160209     C                   PARM      *blanks       UBTA4ITLA
067400160209     C                   PARM      tasAAS        UBTA4IAAS
067500160209     C                   PARM      tasLNP        UBTA4ILNP
067600160209     C                   PARM      tasNRS        UBTA4INRS
067700160209     C                   PARM      tasNSP        UBTA4INSP
067800160209     C                   PARM                    UBTA4ITRC
067900160209     C                   PARM                    UBTA4OERR
068000160209     C                   PARM                    UBTA4ODS
068100160209     C                   PARM                    UBTA4OLEN
068200160209     C                   PARM                    UBTA4ODATI
068300160209     C*
068400160209     C                   IF        UBTA4OERR = *zeros
068500160209     C                   SELECT
068600160209     C* Gestione output tipo record 'A'
068700160209     C                   WHEN      UBTA4ODS = 'DTA4A'
068800160209     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
068900160209     C* Gestione output tipo record 'M'
069000160209     C                   WHEN      UBTA4ODS = 'DTA4M'
069100160209     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
069200160209     C                   ENDSL
069300160209     C*
069400160209     C                   ENDIF
069500160209     C*
069600160209     C                   ENDSR
069700151016
069800151016
069900151016
070000090424     C*----------------------------------------------------*
070100090424     C     *INZSR        BEGSR
070200090424     C*
070300090424     C     *ENTRY        PLIST
070400090424     C                   PARM                    KPJBA
070500090424     C                   MOVEL     KPJBU         PARAM
070600090424      *
070700090424      * Calcola la data corrente
070800150917     C                   z-add     *zeros        datcor            8 0
070900150917     C                   eval      datcor = %dec(%date() : *ISO)
071000090424      *
071100090424      * Se nn già ricevute nei parametri d lancio calcolo le date DA/A
071200090424     C                   z-add     *hival        wDataDa           8 0
071300090424     C                   z-add     *hival        wDataA            8 0
071400090424     C                   if        PARDATADA > *zeros AND
071500090424     C                             PARDATAA  > *zeros
071600090424     C                   z-add     PARDATADA     wDataDa
071700090424     C                   z-add     PARDATAA      wDataA
071800090424     C                   else
071900090424     C                   eval      wDataDa=%dec(%date(datcor)-
072000090424     C                                     %days(PARGGBACK))
072100090424     C                   eval      wDataA =%dec(%date(wDataDa)+
072200090424     C                                     %days(PARGGDAELAB))
072300090424     C                   endif
072400090424      *
072500090424      * Verifico i parametri ricevuti in ingresso
072600090424     C                   if        PARCLIVGD = *zeros
072700090424     C                   movel     *all'0'       PARCLIVGD
072800090424     C                   move      PARCLI        PARCLIVGD
072900090424     C                   endif
073000090424      *
073100090424     C* TABEL
073200090424     C     KTB           KLIST
073300090424     C                   KFLD                    TBLKUT
073400090424     C                   KFLD                    TBLCOD
073500150917     C* FIAR
073600150917     C     kFiar5        Klist
073700150917     C                   Kfld                    TasAas
073800150917     C                   Kfld                    TasLnp
073900150917     C                   Kfld                    TasNrs
074000150917     C                   Kfld                    TasNsp
074100150917     C                   Kfld                    kAr5Trd
074200090424     C* TITAS
074300090424     C     KTAS          KLIST
074400090424     C                   KFLD                    KSC
074500090424     C* TITA7
074600090424     C     KTA7          KLIST
074700090424     C                   KFLD                    TASAAS
074800090424     C                   KFLD                    TASLNP
074900090424     C                   KFLD                    TASNRS
075000090424     C                   KFLD                    TASNSP
075100090424     C                   KFLD                    TASTBL
075200150917     C*
075300090424     C     *LIKE         DEFINE    TASVA1        WVA
075400090424     C     *LIKE         DEFINE    TASSV1        WSV
075500090424     C*
075600090424     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
075700090424     C     KORM          KLIST
075800160209     C                   KFLD                    §TA4MPOE
075900160209     C                   KFLD                    §TA4MNSR
076000160209     C                   KFLD                    §TA4MNOR
076100160209     C                   KFLD                    §TA4MNRV
076200090424     C*
076300090424     C* LEGGE IL RECORD SIGLE VARIE
076400090424     C     *LIKE         DEFINE    TBLKUT        KTBKUT
076500090424     C     *LIKE         DEFINE    TBLCOD        KTBCOD
076600090424     C     *LIKE         DEFINE    TBLKEY        KTBKEY
076700090424     C     KEYTAB        KLIST
076800090424     C                   KFLD                    KTBKUT
076900090424     C                   KFLD                    KTBCOD
077000090424     C                   KFLD                    KTBKEY
077100090424     C*
077200090424     C                   ENDSR
077300090423**         VALNUM
0774000904230123456789
