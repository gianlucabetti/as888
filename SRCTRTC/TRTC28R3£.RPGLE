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
001500150917
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
002700150922
002800150922
002900150922     D* Strutture per passaggio parametri a driver per calcolo tassazione
003000150922     D IN_PF_OPZ       S              1    INZ(*blanks)
003100160530     D IN_PF_DSTAS   E DS                  EXTNAME(titas00f)
003200160530     D OUT_PF_DTAS   E DS                  EXTNAME(DTAS)    PREFIX(PF_)
003300160530     D OUT_PF_DTASV  E DS                  EXTNAME(DTASV)   PREFIX(PF_)
003400160503     D  TSV                    1     20                                         (T = da TNSF22R)
003500150922     D                                     DIM(20)                              Sigle varie
003600160503     D  TVA                   21    140P 3                                      (T = da TNSF22R)
003700150922     D                                     DIM(20)                              Importi varie
003800160530     D OUT_PF_DTASPESE DS                  EXTNAME(DTASPES) PREFIX(PF_)
003900150922
004000150922
004100090423     D PARAM           DS
004200090423     D  PARDATADA                     8S 0
004300090423     D  PARDATAA                      8S 0
004400090424     D  PARCLI                        7S 0
004500090424     D  PARTIPCLI                     1A
004600090423     D  PARCLIVGD                     8A
004700090424     D  PARTIPFIL                     2A
004800090424     D  PARGGBACK                     3S 0
004900090424     D  PARGGDAELAB                   3S 0
005000150922     D  PARTYPE                       1A                                        A=typf AF P=typf PF
005100150921     D  PARFORZAFATT                  1A
005200150922
005300150922     D* DS FIAR5
005400150922     D dAr5Bnb       e ds
005500051219
005600150922     D Og143         e ds                  inz
005700150922     D*
005800021212     D Wtara           s                   like(§QTTPC)
005900021212     D Wntara          s                   like(TASPKF)
006000030917
006100150922     D kAr5Trd         s                   Like(Ar5Trd)
006200030515
006300030515     D Rma15           s             15
006400030515     D WRma15          s             15
006500030515     D II              s              2  0
006600030515     D Pos             s              2  0
006700160209
006800160209     D*--------------------
006900160209     D* DEFINIZIONI A PROCEDURE ESTERNE
007000160209     D*--------------------
007100160209     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
007200160209
007300010402
007400010402
007500170227     C*
007600170227     C* Inizializzo variabili d wrk
007700170227     C                   movel     'N'           wProcedi          1
007800170227     C*
007900170227     C* Inizializzo la transazione
008000170227     C                   CLEAR                   TIS7VASDS
008100170227     C                   EVAL      i§7VASOPZ = 'PRG'
008200170227     C                   CALL(e)   'TIS7VASR1'
008300170227     C                   PARM                    TIS7VASDS
008400170227     C*
008500170227     C* Verifico esito chiamata
008600170227     C                   if        not %error AND
008700170227     C                             o§7VASOK = *on AND o§7VASPRG <> *blanks
008800170227     C                   movel     'S'           wProcedi
008900170227     C                   endif
009000170227     C*
009100170227     C* Se ok a procedere => elaboro
009200170227     C                   if        wProcedi = 'S'
009300060127     C*
009400921023     C                   EXSR      DEFCAM
009500090424      *
009600090424      * Gestisco se richiesto cliente SINGOLO o UNIFICANTE
009700090424     C                   if        PARTIPCLI = 'U'
009800040806      *
009900921023     C     KTB           SETLL     TABEL
010000040806    1C                   DO        *HIVAL
010100921023     C     KTB           READE     TABEL                                  58
010200040806    2C  N58              DO
010300040806    3C     TBLFLG        IFNE      '*'
010400921023     C                   MOVEL     TBLUNI        DS4C
010500040806    4C     §4CFSD        IFNE      *BLANK                                        CLI DA SCAR
010600050906     C                   MOVE      §4CFKS        KSCU              7 0
010700050906     C                   MOVEL     TBLKEY        KSC               7 0
010800981203     C*
010900090424    5C     PARCLI        IFGT      *ZERO                                         PARZIALIZZ.
011000090424     C     KSCU          ANDEQ     PARCLI                                        CLIENTE
011100000721     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
011200090424   e5C                   ENDIF
011300090424   e4C                   ENDIF
011400090424   e3C                   ENDIF
011500090424   e2C                   ENDDO
011600090424   e1C  N58              ENDDO
011700060127     C*
011800090424     C                   else
011900090424     C                   eval      KSC = PARCLI
012000090424     C                   EXSR      WRTFIL                                        LEGGE/SCRIVE
012100090424     C                   endif
012200090424     C*
012300060127     C                   endif
012400170227     C*
012500170227     C* Finalizzo la transazione
012600170227     C                   EVAL      i§7VASOPZ  = 'RLS'
012700170227     C                   EVAL      i§7VASTIP  = PARTIPFIL
012800170227     C                   EVAL      i§7VASKSU  = PARCLIVGD
012900170227     C                   EVAL      i§7VASTSC  = 'WW'
013000170227     C                   EVAL      i§7VASSTO  = '?'
013100170227     C                   EVAL      i§7VASSTTO = 'G'
013200170227     C                   EVAL      i§7VASPRG  = o§7VASPRG
013300170227     C                   CALL(e)   'TIS7VASR1'
013400170227     C                   PARM                    TIS7VASDS
013500170227     C*
013600170227     C* Verifico esito chiamata
013700170227     C                   if        %error OR o§7VASOK = *off
013800170227     C                   exsr      EXEERR
013900170227     C                   endif
014000160209     C*
014100160209     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
014200160209     C                   CALL      'UBTA400R'
014300160209     C                   PARM                    UBTA4IOPZ
014400160209     C                   PARM      'C'           UBTA4ITLA
014500160209     C                   PARM                    UBTA4IAAS
014600160209     C                   PARM                    UBTA4ILNP
014700160209     C                   PARM                    UBTA4INRS
014800160209     C                   PARM                    UBTA4INSP
014900160209     C                   PARM                    UBTA4ITRC
015000160209     C                   PARM                    UBTA4OERR
015100160209     C                   PARM                    UBTA4ODS
015200160209     C                   PARM                    UBTA4OLEN
015300160209     C                   PARM                    UBTA4ODATI
015400991110     C*
015500921023     C                   SETON                                        LR
015600910830     C*----------------------------------------------------*
015700910830     C     DEFCAM        BEGSR
015800021212      *
015900021212      ** Aggancio tabella $2 - Varie forzate a programma
016000990817     C                   Z-ADD     1             KTBKUT
016100990817     C                   MOVEL     '$2'          KTBCOD
016200990817     C                   MOVEL     1             KTBKEY
016300990817     C     KEYTAB        CHAIN     TABEL00F                           99
016400990817IF  1C     *IN99         IFEQ      *OFF
016500990817     C     TBLFLG        ANDEQ     *BLANKS
016600990817     C                   MOVEL     TBLUNI        DS$2
016700990817E   1C                   ENDIF
016800021212      *
016900050905      ** Aggancio tabella QT - campi standard tassazione
017000021212     C                   MOVEL     'QT'          KTBCOD
017100021212     C                   MOVEL     1             KTBKEY
017200021212     C     KEYTAB        CHAIN     TABEL00F                           99
017300021212IF  1C     *IN99         IFEQ      *OFF
017400021212     C     TBLFLG        ANDEQ     *BLANKS
017500021212     C                   MOVEL     TBLUNI        DSQT1
017600021212E   1C                   ENDIF
017700990817     C*
017800021212      ** Imposto key per tabella clienti con ritorno info fattura
017900990817     C                   MOVE      1             TBLKUT
018000990817     C                   MOVEL     '4C'          TBLCOD
018100990817     C                   MOVEL     *BLANKS       TBLKEY
018200990817     C*
018300910830     C                   ENDSR
018400050906     C*--------------------------------------------------------
018500090423     C* WRTFIL    LEGGE TITAS00F/TITAS10F E SCRIVE TIVAF00T   *
018600000124     C*--------------------------------------------------------
018700921023     C     WRTFIL        BEGSR
018800090423     C*
018900090424     C* Mi posiziono x cliente
019000090423     C                   setoff                                       70
019100090423     C*
019200090424     C     KTAS          SETLL     TITAS31C
019300090424     C                   IF        %found(titas31C)
019400090424     C                   READ      titas31c
019500090424     C                   DOW       not %eof(titas31c) and not *in70
019600090423     C*
019700150917     C* Verifico se il cliente è quello richiesto
019800090424     C                   if        tasKSC <> KSC
019900090423     C                   seton                                        70
020000090423     C                   else
020100150917     C*
020200150917     C* Verifico se data spedizione è nel periodo richiesto
020300090424     C                   if        tasAAS*10000+tasMGS >= wDataDa AND
020400090424     C                             tasAAS*10000+tasMGS <= wDataA
020500150922     C*
020600150922     C* Gestisco se nei parametri in input:
020700150922     C*    - se richiesto lancio come A = ANTEPRIMA  FATTURE (AF)
020800150922     C*    - se richiesto lancio come P = PREVISIONE FATTURE (PF)
020900150922     C*    - il default è                 ANTEPRIMA  FATTURE (AF)
021000150922     C*
021100150922     C                   SELECT
021200150922     C                   WHEN      PARTYPE = 'A'
021300150922     C                   MOVEL     '1'           wInvoiced
021400150922     C*
021500150922     C                   WHEN      PARTYPE = 'P'
021600150917     C*
021700150922     C* Inizializzo il flag che indica se spedizione già fatturata o meno
021800150921     C                   MOVEL     '0'           wInvoiced         1
021900150917     C                   IF        TASDFT > *zeros OR TASNFT > *zeros
022000150917     C                   MOVEL     '1'           wInvoiced
022100150917     C                   ENDIF
022200150921     C*
022300150921     C* Se richiesto nei parametri forzo lo stato di "spedizione già fatturata o meno"
022400150921     C                   IF        PARFORZAFATT <> *blanks
022500150921     C                   SELECT
022600150921     C                   WHEN      PARFORZAFATT = 'S'
022700150921     C                   MOVEL     '1'           wInvoiced
022800150921     C                   WHEN      PARFORZAFATT = 'N'
022900150921     C                   MOVEL     '0'           wInvoiced
023000150921     C                   ENDSL
023100150921     C                   ENDIF
023200150922     C*
023300150922     C                   OTHER
023400150922     C                   MOVEL     '1'           wInvoiced
023500150922     C                   ENDSL
023600150921     C*
023700150921     C* Inizializzazioni varie
023800150921     C                   MOVE      *ZERO         WSV
023900150921     C                   MOVE      *ZERO         WVA
024000150921     C                   MOVEA     *BLANKS       VSV
024100150921     C                   MOVEA     *ZERO         VVA
024200150921     C                   CLEAR                   A
024300150921     C                   CLEAR                   VAFSV1
024400150921     C                   CLEAR                   VAFVA1
024500150921     C                   CLEAR                   VAFSV2
024600150921     C                   CLEAR                   VAFVA2
024700150921     C                   CLEAR                   VAFSV3
024800150921     C                   CLEAR                   VAFVA3
024900150921     C                   CLEAR                   VAFSV4
025000150921     C                   CLEAR                   VAFVA4
025100150921     C                   CLEAR                   VAFSV5
025200150921     C                   CLEAR                   VAFVA5
025300150921     C                   CLEAR                   VAFSV6
025400150921     C                   CLEAR                   VAFVA6
025500150921     C                   CLEAR                   VAFSV7
025600150921     C                   CLEAR                   VAFVA7
025700150921     C                   CLEAR                   VAFSV8
025800150921     C                   CLEAR                   VAFVA8
025900150921     C                   CLEAR                   VAFVAX
026000150917     C*
026100150917     C* ***********
026200150917     C* Se spedizione non ancora fatturata =>
026300150917     C* ***********
026400150922     C                   if        wInvoiced = *off
026500150922     C*
026600150922     C                   call      'UBPREFATR'
026700150922     C                   parm                    IN_PF_OPZ
026800150922     C                   parm                    IN_PF_DSTAS
026900150922     C                   parm                    OUT_PF_DTAS
027000150922     C                   parm                    OUT_PF_DTASV
027100151020     C                   parm                    OUT_PF_DTASPES
027200150922     C
027300150922     C                   exsr      VALDTAS
027400150921     C*
027500150922     C                   endif
027600160530     C*
027700160530     C* ***********
027800160530     C* Reperisco tutti i dati necessari non presenti in forma diretta su TITAS
027900160530     C* ***********
028000160530     C*
028100160530     C* Se particolità varia = 'O' recupero da FIAR5 il numero colli originali
028200160530     C                   If        %Subst(TASGVA:1:1) = 'O'
028300160530     C                   eval      kAr5Trd  = 'BNB'
028400160530     C                   clear                   dAr5Bnb
028500160530     C     kFiar5        Chain     Fiar531c
028600160530     C                   If        %Found(Fiar531c)
028700160530     C                   Movel     Ar5Uni        dAr5Bnb
028800160530     C                   EndIf
028900160530     C                   Eval      TASNCL = §Ar5bcor                            Colli originali
029000160530     C                   endif
029100050905     C*
029200050905     C* Inizializzo il buffer d output
029300050905     C                   clear                   TIVAFDS
029400160209     C*
029500160209     C* Reperisco TITA4 tipo record 'A'
029600160209     C                   MOVEL     'A'           UBTA4ITRC
029700160209     C                   EXSR      RTVTA4
029800150917     C*
029900150917     C                   Z-ADD     TASNCL        VAFNCL
030000050905     C*
030100941122     C                   MOVEL     TASTBL        VAFTBL
030200941122     C                   Z-ADD     TASLNP        VAFLNP
030300941122     C                   Z-ADD     TASMGS        VAFMGS
030400941122     C                   Z-ADD     TASAAS        VAFAAS
030500941122     C                   Z-ADD     TASNRS        VAFNRS
030600941122     C                   Z-ADD     TASNSP        VAFNSP
030700941122     C                   Z-ADD     TASKSC        VAFKSC
030800941122     C                   MOVEL     TASFIN        VAFFIN
030900941122     C                   Z-ADD     TASLNA        VAFLNA
031000941122     C                   Z-ADD     TASVLF        VAFVLF
031100941122     C                   Z-ADD     TASPOR        VAFPOR
031200150921     C* PESO
031300021211     C                   Z-ADD     TASPKF        VAFPKB
031400021211     C                   IF        TASFPF = 'V'
031500150716     C*
031600150716     C                   IF        TASDFT <= §QTDST
031700150716     C     §QTTCO        MULT      TASNCP        WTARA
031800150716     C     TASPKC        SUB(h)    WTARA         WNTARA
031900150716     C                   Z-ADD     WNTARA        VAFPKC
032000150716     C                   ELSE
032100150716     C     §QTTPC        MULT      TASNCP        WTARA
032200150716     C     TASPKC        SUB(h)    WTARA         WNTARA
032300150716     C                   Z-ADD     WNTARA        VAFPKC
032400150716     C                   ENDIF
032500150716     C*
032600021211     C                   ELSE
032700151020     C                   IF        TASFPF <> 'D'
032800150716     C                   Z-ADD     *zeros        VAFPKC
032900151020     C                   ENDIF
033000021211     C                   ENDIF
033100000124     C*
033200150922     C* Se trattasi di spedizione già fatturata
033300150922     C                   IF        wInvoiced = *on
033400150917     C*
033500150917     C                   MOVEL     TASSV1        WSV
033600990910     C                   Z-ADD     TASVA1        WVA
033700990910     C                   EXSR      CONTR
033800990910     C                   MOVEL     TASSV2        WSV
033900990910     C                   Z-ADD     TASVA2        WVA
034000990910     C                   EXSR      CONTR
034100990910     C                   MOVEL     TASSV3        WSV
034200990910     C                   Z-ADD     TASVA3        WVA
034300990910     C                   EXSR      CONTR
034400150917     C*
034500150917     C     TASSV3        IFNE      *BLANK
034600150917     C                   EXSR      SRTA7
034700150917     C                   ENDIF
034800990817     C*
034900941122     C                   Z-ADD     TASDFT        VAFDFT
035000941122     C                   Z-ADD     TASNFT        VAFNFT
035100150917     C*
035200150922     C* Se trattasi di spedizione NON già fatturata
035300150921     C                   ELSE
035400150921     C                   EXSR      TVA2CONTR
035500150921     C*
035600150922     C                   ENDIF
035700150917     C*
035800150917     C                   if        tascbo <> 'FO'
035900941122     C                   Z-ADD     TASRMN        VAFRMN
036000150917     C                   endif
036100150917     C*
036200941122     C                   MOVEL     TASFTC        VAFFTC
036300941122     C                   MOVEL     TASTSP        VAFTSP
036400941122     C                   MOVEL     TASCTS        VAFCTS
036500941122     C                   MOVEL     TASFAP        VAFFAP
036600941122     C                   Z-ADD     TASCTR        VAFCTR
036700941122     C                   Z-ADD     TASQFT        VAFQFT
036800941122     C                   MOVEL     TASTC2        VAFTC2
036900000124     C                   Z-ADD     TASIMV        VAFIMV
037000000124     C                   Z-ADD     TASFIV        VAFFIV
037100000124     C                   MOVEL     TASDIV        VAFDIV
037200000124     C                   Z-ADD     TASDRT        VAFDRT
037300160209     C                   MOVEL     §TA4ANAS      VAFNAS
037400000124     C                   MOVEL     TASCAD        VAFCAD
037500000124     C                   MOVEL     TASLOD        VAFLOD
037600000124     C                   MOVEL     TASPRD        VAFPRD
037700000124     C                   MOVEL     TASNZD        VAFNZD
037800000124     C                   MOVEL     TASVAS        VAFVAS
037900000124     C                   Z-ADD     TASIAS        VAFIAS
038000000124    *C                   MOVEL     *blanks       VAFFPC
038100000124    *C                   MOVEL     *blanks       VAFFVC
038200000124    *C                   Z-ADD     *zeros        VAFVLC
038300030515     C                   MOVEL     *BLANKS       VAFRMA
038400150921     C* Se codice bolla diverso da 'FO'(richiesta bolla firmata)valorizzo RMA con RMA
038500160222     C                   if        tascbo <> 'FO'
038600160209     C                   MOVEL     DTA4A         VAFRMA
038700000124     C                   ENDIF
038800150921     C* Se codice bolla uguale a 'FO'(richiesta bolla firmata)valorizzo RMA con RMN e viceversa
038900160222     C                   if        tascbo = 'FO'
039000150921     C                   if        §ta4arma <> *blanks
039100150921     C                   MOVEL     §TA4ARMA      Rma15
039200150921     C* Verifico se campo numerico escludendo i blanks
039300150921     C                   eval      II = 1
039400150921     C                   dow       II <= %len(%trim(Rma15))
039500150921     C*
039600150921     C                   If        %subst(%trim(Rma15):II:1) <> *blank
039700150921     C                   eval      WRma15 = (%trim(Wrma15)
039800150921     C                                      + %subst(%trim(Rma15):II:1))
039900150921     C                   endif
040000150921     C*
040100150921     C                   eval      II = II + 1
040200150921     C*
040300150921     C                   enddo
040400150921     C                   clear                   rma15
040500150921     C                   evalr     rma15 = %trim(Wrma15)
040600150921     C*
040700150921     C                   eval      II = 1
040800150921     C                   setoff                                       29
040900150921     C                   dow       II <= %len(%trim(Rma15))
041000150921     C*
041100150921     C                   If        %subst(%trim(Rma15):II:1) <> *blank
041200150921     C                   eval      pos = %scan(%subst(%trim(Rma15):II:1):
041300150921     C                             VALNUM(1))
041400150921     C                   if        pos = 0
041500150921     C                   seton                                        29
041600150921     C                   leave
041700150921     C                   endif
041800150921     C                   endif
041900150921     C*
042000150921     C                   eval      II = II + 1
042100150921     C*
042200150921     C                   enddo
042300150921     C                   if        *in29 = *OFF
042400150921     C                   eval      VAFRMA= %editc(tasrmn:'Z')
042500150921     C                   MOVEL     rma15         VAFRMN
042600150921     C                   else
042700150921     C                   EVAL      VAFRMA = §ta4arma
042800150921     C                   EVAL      VAFRMN = tasrmn
042900150921     C                   endif
043000150921     C*
043100150921     C                   endif
043200030515     C                   endif
043300990910     C*
043400990910     C* SCRIVO LE VARIE
043500990910     C                   Z-ADD     1             A
043600990910     C     VSV(A)        DOWNE     ' '
043700990910     C                   SELECT
043800990910     C     VAFSV1        WHENEQ    ' '
043900990910     C                   MOVEL     VSV(A)        VAFSV1
044000990910     C                   Z-ADD     VVA(A)        VAFVA1
044100990910     C*
044200990910     C     VAFSV2        WHENEQ    ' '
044300990910     C                   MOVEL     VSV(A)        VAFSV2
044400990910     C                   Z-ADD     VVA(A)        VAFVA2
044500990910     C*
044600990910     C     VAFSV3        WHENEQ    ' '
044700990910     C                   MOVEL     VSV(A)        VAFSV3
044800990910     C                   Z-ADD     VVA(A)        VAFVA3
044900000124     C*
045000000124     C     VAFSV4        WHENEQ    ' '
045100000124     C                   MOVEL     VSV(A)        VAFSV4
045200000124     C                   Z-ADD     VVA(A)        VAFVA4
045300000124     C*
045400000124     C     VAFSV5        WHENEQ    ' '
045500000124     C                   MOVEL     VSV(A)        VAFSV5
045600000124     C                   Z-ADD     VVA(A)        VAFVA5
045700000124     C*
045800000124     C     VAFSV6        WHENEQ    ' '
045900000124     C                   MOVEL     VSV(A)        VAFSV6
046000000124     C                   Z-ADD     VVA(A)        VAFVA6
046100000124     C*
046200000124     C     VAFSV7        WHENEQ    ' '
046300000124     C                   MOVEL     VSV(A)        VAFSV7
046400000124     C                   Z-ADD     VVA(A)        VAFVA7
046500000124     C*
046600000124     C     VAFSV8        WHENEQ    ' '
046700000124     C                   MOVEL     VSV(A)        VAFSV8
046800000124     C                   Z-ADD     VVA(A)        VAFVA8
046900990910     C                   OTHER
047000990910     C                   ADD       VVA(A)        VAFVAX
047100990910     C                   ENDSL
047200990910     C*
047300990910     C                   ADD       1             A
047400150921     C                   ENDDO
047500150921     C* Imposto campo VAFRMX in base alla tabella 4C o se cliente DPD
047600150921     C                   Clear                   VafRmx
047700150921     C                   MOVEL     VAFKSC        WLB3              3 0
047800150921     C     WLB3          chain     AZORG
047900150921     C                   if            %found(AZORG01L)
048000150921     C                             and ORGfva = *blanks
048100150921     C                   movel     ORGde3        Og143
048200150921     C                   else
048300150921     C                   clear                   Og143
048400150921     C                   endif
048500150921     C*
048600150921     C                   Select
048700150921     C*
048800150921     C* Recupera Riferimento Alfanumerico da ORM
048900050126     C                   When      §4cRfo = 'S'
049000160209     C*
049100160209     C* Reperisco TITA4 tipo record 'M'
049200160209     C                   MOVEL     'M'           UBTA4ITRC
049300160209     C                   EXSR      RTVTA4
049400160209     C*
049500160209     C                   IF        DTA4M <> *zeros
049600050126     C     KORM          CHAIN     FNORM01L                           61
049700050126     C     *IN61         IFEQ      *OFF
049800050126     C                   MOVEL     ORMRFA        VAFRMX
049900050126     C                   ENDIF
050000050126     C                   ENDIF
050100160209     C*
050200150921     C* Ritorna al cliente il campo XCO
050300150921     C***                When      §4cRfo = 'X' and TasXco <> *Blanks
050400150921     C* recupero il valore da tornare dalla tabella VUS
050500150921     C***                Clear                   Tibs02ds
050600150921     C***                Clear                   dvus
050700150921     C***                Eval      T02mod = 'C'
050800150921     C***                Eval      T02sif = knsif
050900150921     C***                Eval      T02cod = 'VUS'
051000150921     C***                Eval      T02ke1 = TasXco
051100150921     C***                Call      'TIBS02R'
051200150921     C***                Parm                    kpjba
051300150921     C***                Parm                    Tibs02ds
051400150921     C***                If        T02err = *blanks
051500150921     C***                Eval      dvus = T02uni
051600150921     C***                EndIf
051700150921     C***                Eval      vafrmx = §vuvaf
051800150921     C*
051900150921     C* PEZZA PER METTERE DATA CONSEGNA PER CONTROLLO DPD
052000150921     C                   When      §OGntw = 'DPD'
052100150921     C                   movel     TASDCM        VAFRMX
052200150921     C*
052300150921     C                   EndSl
052400151016     C*
052500151016     C* Eseguo eventuali "forzature"
052600151016     C                   EXSR      RTVAR5FAT
052700151020     C                   EXSR      CHKPESDES
052800060203     C*
052900060203     C* Quindi scarico il buffer nel file d out generico download
053000050905     C                   exsr      WRIVGD
053100050905     C*
053200090423     C                   endif
053300090424     C                   endif
053400090423     C*
053500090423     C* Proseguo la lettura delle bolle
053600090424     C                   read      titas31c
053700090423     C*
053800090423     C                   ENDDO
053900090423     C                   ENDIF
054000090423     C*
054100000124     C                   ENDSR
054200150922     C*---------------------------------------------------------------*
054300150922     C*  VALORIZZAZIOEN CAMPI DS DTAS DA CAMPI TITAS IN INPUT
054400150922     C*---------------------------------------------------------------*
054500150922     C     VALDTAS       BEGSR
054600150922     C*
054700150922     C                   eval      TASKSC  = PF_TASKSC
054800150922     C                   eval      TASPKF  = PF_TASPKF
054900150922     C                   eval      TASPKC  = PF_TASPKC
055000150922     C                   eval      TASNCP  = PF_TASNCP
055100150922     C                   eval      TASVLF  = PF_TASVLF
055200150922     C                   eval      TASNCL  = PF_TASNCL
055300150922     C                   eval      TASCTR  = PF_TASCTR
055400150922     C                   eval      TASCTS  = PF_TASCTS
055500150922     C                   eval      TASNZD  = PF_TASNZD
055600150922     C                   eval      TASCAD  = PF_TASCAD
055700150922     C                   eval      TASFIN  = PF_TASFIN
055800150922     C                   eval      TASFDN  = PF_TASFDN
055900150922     C                   eval      TASMCT  = PF_TASMCT
056000150922     C                   eval      TASNZM  = PF_TASNZM
056100150922     C                   eval      TASCAM  = PF_TASCAM
056200150922     C                   eval      TASFAP  = PF_TASFAP
056300150922     C                   eval      TASTSP  = PF_TASTSP
056400150922     C                   eval      TASTC2  = PF_TASTC2
056500150922     C                   eval      TASLNP  = PF_TASLNP
056600150922     C                   eval      TASLNA  = PF_TASLNA
056700150922     C                   eval      TASTBL  = PF_TASTBL
056800150922     C                   eval      TASVAS  = PF_TASVAS
056900150922     C                   eval      TASIAS  = PF_TASIAS
057000150922     C                   eval      TASQFT  = PF_TASQFT
057100150922     C                   eval      TASDIV  = PF_TASDIV
057200150922     C                   eval      TASPOR  = PF_TASPOR
057300150922     C                   eval      TASIMV  = PF_TASIMV
057400150922     C                   eval      TASPVL  = PF_TASPVL
057500150922     C*
057600150922     C                   ENDSR
057700000124     C*-----------------------------------------------------------*
057800000124     C* SOMMA IL VALORE DELLE VARIE DI TAS7 OLTRE LA 8° IN VAFVAX *
057900000124     C*-----------------------------------------------------------*
058000941122     C     SRTA7         BEGSR
058100990817     C*
058200990823     C     KTA7          SETLL     TITA730C
058300990823     C     KTA7          READE     TITA730C                               59
058400990817DO  1C     *IN59         DOWEQ     *OFF
058500990910     C                   MOVEL     TA7SVN        WSV
058600990910     C                   Z-ADD     TA7VAN        WVA
058700990910     C                   EXSR      CONTR
058800990910     C*
058900990823     C     KTA7          READE     TITA730C                               59
059000990817E   1C                   ENDDO
059100990817     C*
059200941122     C                   ENDSR
059300990910     C*--------------------------------------------------------
059400000124     C* CONTROLLO SE VARIE GIA' PRESENTI
059500000124     C*--------------------------------------------------------
059600990910     C     CONTR         BEGSR
059700000124IF  1C     WSV           IFNE      ' '
059800990910     C*
059900990910IF  4C     WSV           IFNE      §$2BOL                                       *NO BOLLA e NO IVA
060000990910     C     WSV           ANDNE     §$2IVA
060100990910     C                   ADD       1             A                 3 0
060200990910     C                   MOVEL     WSV           VSV(A)
060300990910     C                   Z-ADD     WVA           VVA(A)
060400990910E   4C                   ENDIF
060500000124E   1C                   ENDIF
060600000124     C                   ENDSR
060700150921     C*--------------------------------------------------------
060800150921     C* CARICAMENTO "CONTR" DA PREVISIONE TASSAZIONE
060900150921     C*--------------------------------------------------------
061000150921     C     TVA2CONTR     BEGSR
061100150921     C*
061200150921     C                   DOW       A < %elem(TSV)
061300150921     C                   ADD       1             A                 3 0
061400150921     C                   MOVEL     TSV(A)        VSV(A)
061500150921     C                   Z-ADD     TVA(A)        VVA(A)
061600150921     C                   ENDDO
061700150921     C*
061800150921     C                   ENDSR
061900090423
062000090423
062100050905
062200050905      /TITLE Scrittura record TIVAF in file TIVGD00F (file VAS generico download)
062300050905     C     wriVGD        BEGSR
062400050905     C*
062500050905     C                   clear                   tivgd000
062600090423     C                   eval      vgdDTA = TIVAFDS
062700090424     C                   eval      vgdTIP = PARTIPFIL
062800090423     C                   eval      vgdKSU = PARCLIVGD
062900090423     C                   eval      vgdTSC = 'WW'
063000050905     C                   eval      vgdDAT = datcor
063100170227     C                   eval      vgdPRG = o§7VASPRG
063200170227     C                   eval      vgdPGM = procname
063300170227     C                   eval      vgdSTO = '?'
063400050905     C                   write     tivgd000
063500050905     C*
063600050905     C                   ENDSR
063700151016
063800151016
063900151016
064000151020     C*--------------------------------------------------------
064100151016     C     RTVAR5FAT     BEGSR
064200151016     C*
064300151020     C* Se, presente, considero il peso/volume DESUNTO (già fatturato) al pari del VDL TOTALE
064400151028     C                   SETOFF                                       7172
064500151016     C                   EVAL      kAr5Trd = 'FAT'
064600151016     C                   CLEAR                   DAR5FAT
064700151016     C     kFiar5        CHAIN     FIAR531C
064800151016     C                   IF        %found(FIAR531C)
064900151016     C                   MOVEL     AR5UNI        DAR5FAT
065000170130     C                   IF        (§AR5FPTAS = 'T' OR §AR5FPTAS = 'D') AND
065100170130     C                             §AR5PKTAS > *zeros
065200170130     C                   SETON                                        71
065300170130     C                   EVAL      VAFPKC = §AR5PKTAS
065400170130     C                   ENDIF
065500170130     C                   IF        (§AR5FVTAS = 'T' OR §AR5FVTAS = 'D') AND
065600170130     C                             §AR5VLTAS > *zeros
065700170130     C                   SETON                                        72
065800170130     C                   EVAL      VAFVLC = §AR5VLTAS
065900170130     C                   ENDIF
066000151016     C                   ENDIF
066100151016     C*
066200151016     C                   ENDSR
066300151020     C*--------------------------------------------------------
066400151020
066500151020
066600151020
066700151020     C*--------------------------------------------------------
066800151020     C     CHKPESDES     BEGSR
066900151020     C*
067000151028     C                   SETOFF                                       73
067100151020     C*
067200151020     C* Se, presente, considero il peso/volume DESUNTO (calcolato run-time) al pari del VDL TOTALE
067300151020     C                   IF        PF_taspDES = 'S'
067400151020     C                   IF        PF_taspPKD > *zeros
067500151028     C                   SETON                                        73
067600151020     C                   EVAL      VAFPKC = PF_taspPKD
067700151020     C                   ENDIF
067800151020     C                   ENDIF
067900151020     C*
068000151020     C                   ENDSR
068100151020     C*--------------------------------------------------------
068200160209
068300160209
068400160209
068500160209     C     RTVTA4        BEGSR
068600160209     C*
068700160209     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
068800160209     C                   CLEAR                   DTA4A
068900160209     C                   CLEAR                   DTA4M
069000160209     C*
069100160209     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
069200160209     C                   CALL      'UBTA400R'
069300160209     C                   PARM      *blanks       UBTA4IOPZ
069400160209     C                   PARM      *blanks       UBTA4ITLA
069500160209     C                   PARM      tasAAS        UBTA4IAAS
069600160209     C                   PARM      tasLNP        UBTA4ILNP
069700160209     C                   PARM      tasNRS        UBTA4INRS
069800160209     C                   PARM      tasNSP        UBTA4INSP
069900160209     C                   PARM                    UBTA4ITRC
070000160209     C                   PARM                    UBTA4OERR
070100160209     C                   PARM                    UBTA4ODS
070200160209     C                   PARM                    UBTA4OLEN
070300160209     C                   PARM                    UBTA4ODATI
070400160209     C*
070500160209     C                   IF        UBTA4OERR = *zeros
070600160209     C                   SELECT
070700160209     C* Gestione output tipo record 'A'
070800160209     C                   WHEN      UBTA4ODS = 'DTA4A'
070900160209     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
071000160209     C* Gestione output tipo record 'M'
071100160209     C                   WHEN      UBTA4ODS = 'DTA4M'
071200160209     C                   EVAL      DTA4M = %subst(UBTA4ODATI:1:UBTA4OLEN)
071300160209     C                   ENDSL
071400160209     C*
071500160209     C                   ENDIF
071600160209     C*
071700160209     C                   ENDSR
071800170227
071900170227
072000170227
072100170227     C*------------------------------------------------------------------------*
072200170227     C* EXEERR - Routine di esecuzione azioni su Errore
072300170227     C*------------------------------------------------------------------------*
072400170227     C     EXEERR        BEGSR
072500170227     C*
072600170227     C                   dump(A)
072700170227     C                   rolbk
072800170227     C                   seton                                        lr
072900170227     C                   return
073000170227     C*
073100170227     C                   ENDSR
073200170227     C*------------------------------------------------------------------------*
073300151016
073400151016
073500151016
073600090424     C*----------------------------------------------------*
073700090424     C     *INZSR        BEGSR
073800090424     C*
073900090424     C     *ENTRY        PLIST
074000090424     C                   PARM                    KPJBA
074100090424     C                   MOVEL     KPJBU         PARAM
074200090424      *
074300090424      * Calcola la data corrente
074400150917     C                   z-add     *zeros        datcor            8 0
074500150917     C                   eval      datcor = %dec(%date() : *ISO)
074600090424      *
074700090424      * Se nn già ricevute nei parametri d lancio calcolo le date DA/A
074800090424     C                   z-add     *hival        wDataDa           8 0
074900090424     C                   z-add     *hival        wDataA            8 0
075000090424     C                   if        PARDATADA > *zeros AND
075100090424     C                             PARDATAA  > *zeros
075200090424     C                   z-add     PARDATADA     wDataDa
075300090424     C                   z-add     PARDATAA      wDataA
075400090424     C                   else
075500090424     C                   eval      wDataDa=%dec(%date(datcor)-
075600090424     C                                     %days(PARGGBACK))
075700090424     C                   eval      wDataA =%dec(%date(wDataDa)+
075800090424     C                                     %days(PARGGDAELAB))
075900090424     C                   endif
076000090424      *
076100090424      * Verifico i parametri ricevuti in ingresso
076200090424     C                   if        PARCLIVGD = *zeros
076300090424     C                   movel     *all'0'       PARCLIVGD
076400090424     C                   move      PARCLI        PARCLIVGD
076500090424     C                   endif
076600090424      *
076700090424     C* TABEL
076800090424     C     KTB           KLIST
076900090424     C                   KFLD                    TBLKUT
077000090424     C                   KFLD                    TBLCOD
077100150917     C* FIAR
077200150917     C     kFiar5        Klist
077300150917     C                   Kfld                    TasAas
077400150917     C                   Kfld                    TasLnp
077500150917     C                   Kfld                    TasNrs
077600150917     C                   Kfld                    TasNsp
077700150917     C                   Kfld                    kAr5Trd
077800090424     C* TITAS
077900090424     C     KTAS          KLIST
078000090424     C                   KFLD                    KSC
078100090424     C* TITA7
078200090424     C     KTA7          KLIST
078300090424     C                   KFLD                    TASAAS
078400090424     C                   KFLD                    TASLNP
078500090424     C                   KFLD                    TASNRS
078600090424     C                   KFLD                    TASNSP
078700090424     C                   KFLD                    TASTBL
078800150917     C*
078900090424     C     *LIKE         DEFINE    TASVA1        WVA
079000090424     C     *LIKE         DEFINE    TASSV1        WSV
079100090424     C*
079200090424     C* Chiave primaria su file ORM (Ordini Ritiro Merci)
079300090424     C     KORM          KLIST
079400160209     C                   KFLD                    §TA4MPOE
079500160209     C                   KFLD                    §TA4MNSR
079600160209     C                   KFLD                    §TA4MNOR
079700160209     C                   KFLD                    §TA4MNRV
079800090424     C*
079900090424     C* LEGGE IL RECORD SIGLE VARIE
080000090424     C     *LIKE         DEFINE    TBLKUT        KTBKUT
080100090424     C     *LIKE         DEFINE    TBLCOD        KTBCOD
080200090424     C     *LIKE         DEFINE    TBLKEY        KTBKEY
080300090424     C     KEYTAB        KLIST
080400090424     C                   KFLD                    KTBKUT
080500090424     C                   KFLD                    KTBCOD
080600090424     C                   KFLD                    KTBKEY
080700090424     C*
080800090424     C                   ENDSR
080900090423**         VALNUM
0810000904230123456789
