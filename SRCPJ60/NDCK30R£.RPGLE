000100000000     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(QILE)
000200000000     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME) DBGVIEW(*SOURCE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY/)
000400000000     F*----------
000500000000     F*
000600000000     F*
000700000000     F*----------
000800000000     FNDRIK00F  UP   F  120        DISK
000900000000     FNDRTK01L  O    E             DISK
001000080307D2358X***************FNDRDK01L  UF A E           K DISK
001100080307D2358FNDRDK01L  IF A E           K DISK
001200000000     FNDEFA02L  UF   E           K DISK
001300041005C1790FNDEFA03L  IF   E           K DISK    RENAME(NDEFA000:NDEFA03)
001400000000     FNDREG01L  IF   E           K DISK
001500000000     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600000000     D*----------
001700000000     D* Schiera per XMSGR
001800000000     D $COS            S             27    DIM(1) CTDATA PERRCD(1)
001900000000     D*----------
002000000000     D CST             S             10    DIM(6) CTDATA PERRCD(1)
002100000000     D TIM             S                   DIM(3) LIKE(RTKINSI)
002200000000     D TNU             S                   DIM(3) LIKE(RTKINSN)
002300000000     D FLG             S              2    DIM(4)
002400000000     D COL             S             60    DIM(2) CTDATA PERRCD(1)
002500000000     D*----------
002600000000     D KPJBA         E DS
002700000000     D*----------
002800000000     D NDCKRBRB      E DS
002900000000     D NDCKRB14      E DS
003000000000     D NDCKRB30      E DS
003100000000     D NDCKRB40      E DS
003200000000     D NDCKRB50        DS           120
003300000000D0638X*****  RSNRIF                19     27
003400000000  "  X*****  RSDRIF                33     42
003500000000  "  D  RSNRIF                38     43
003600000000D0638D  RSDRIF                45     50
003700000000     D NDCKRB51      E DS
003800080122D2335D  Spese51               87     91
003900000000     D NDCKRB10      E DS
004000041005C1790D NDRIBA20DS    E DS
004100000000"    D NDRIBA30DS    E DS
004200000000"    D NDRIBA50DS    E DS
004300000000"    D  DSNRFATT              19     27
004400000000"    D  DSDTFATT              33     42
004500041005"    D  DSNRRID               51     59
004600131223"    D* DSimpspese            91     95s 0
004700131223"    D* DSaddspese            96    101
004800041005C1790D NDRIBA70DS    E DS
004900000000     D*----------
005000000000     D NDCK37DS      E DS
005100000000     D XpaOut          DS          2000
005200000000     D*-------------
005300041005C1790D* Chiamata per XDATC
005400041005C1790D  XDATCDS      E DS                  EXTNAME(XDATCDS)
005500000000     D*----------
005600000000     D ANBRADS       E DS                  EXTNAME(ANBRA00F) INZ
005700000000     D $Operazion      S              2
005800000000     D $Struttura      S             10
005900000000     D $LenOut         S              9B 0
006000000000     D $Esito          S              1
006100000000     D*----------
006200000000    >D SOC001        E DS                  EXTNAME(XSOC001DS)
006300000000     D XSOCDS          DS          1000
006400000000     D*----------
006500000000     D XATBDS        E DS
006600000000     D TABI07        E DS                  EXTNAME(ANGI07DS)
006700041118C1790D TAB0E1        E DS                  EXTNAME(ANG0E1DS)
006800000000     D*----------
006900000000     D STATUS         SDS           333
007000000000     D  DSPGM            *PROC
007100000000     D  PARMS            *PARMS
007200000000     D*----------
007300000000     D RECORD          DS
007400000000     D  TIPREC                 2      3
007500000000     D  RIVSIA                17     22
007600000000     D  RIDABI                70     74
007700000000     D  RIDCAB                75     79
007800000000     D  RIACLI               102    107
007900000000     D  RIADIS               104    107
008000000000     D  RIAEMI               108    113
008100000000     D  RIDSOR               114    119
008200000000     D  RIDCDD               120    120
008300000000     D*----------
008400000000     D RIEPIL          DS                  OCCURS(5)
008500000000     D  DNU                    1     15P 0
008600000000     D                                     DIM(3)
008700000000     D  DIM                   16     54P 3
008800000000     D                                     DIM(3)
008900000000     D  DFL                   55     55
009000000000     D*----------
009100000000     D Data10          S               D
009200000000     D Oggi            S               D
009300000000     D EFWDSC          S              6  0
009400000000     D EFWDRP          S              6  0
009500000000     D WWIMSP          S                   LIKE(RTKSBIS)
009600000000     D DataValM        S               D
009700000000     D SAVUnita        S                   LIKE(REGUnita)
009800000000     D SAVSiaNoOk      S                   LIKE(XScSia)
009900000000D0650D $NoEFA          S              1A
010000000000     D*----------
010100000000     D* Testi bottoni risposta di XMSGR
010200000000     D TxtBott         C                   CONST('PROMSG    *LIBL-
010300000000     D                                          COS1203')
010400000000     D*----------
010500000000     D* DS Esterna gestione messaggi per COMIT
010600000000     D XMSGDS        E DS
010700000000     D  STS          E                     EXTFLD(MSGSTS)
010800000000     D                                     DIM(333)
010900000000     D  JBA          E                     EXTFLD(MSGJBA)
011000000000     D                                     DIM(502)
011100000000     D*----------
011200000000     D WWImporto       S                   Like(RDKImpo)
011300000000     D*----------
011400000000     D*----------
011500000000     INDRIK00F  AA  02    2 C1    3 C4
011600000000     I         OR   01
011700000000     I                                  4   10  DISPO         L2
011800000000     I                                114  120  DISPOR        L1  02
011900000000     I                                  1  120  RECORD
012000000000      *
012100000000      *------------------------------------------------------------------*
012200000000      * FLUSSO PRINCIPALE DEL PROGRAMMA                                 *
012300000000      *------------------------------------------------------------------*
012400000000      *
012500000000      * La sequenza dei record è questa:
012600000000      *
012700000000      * RB
012800000000      *    14 20 30 40 50 51 70         (Prima   R.B.)
012900000000      *    14 20 30 40 50 51 70         (Seconda R.B.)
013000000000      *    14 20 30 40 50 51 70         (Terza   R.B.)
013100000000      *
013200000000      *    10 70                         Riep. importi (caus. 37000)
013300000000      *    10 70                         Riep. spese   (caus. 16000)
013400000000      * EF
013500000000      *
013600000000      *------------------------------------------------------------------*
013700000000      *
013800000000     C     TIPREC        CABEQ     '99'          END
013900000000     C     TIPREC        CABEQ     '20'          END
014000000000     C     TIPREC        CABEQ     '70'          END
014100000000     C*
014200000000C1376C* Intesa
014300000000     C     TIPREC        IFEQ      'RB'
014400000000C1376C* CBI
014500000000C1376C     TIPREC        OREQ      'IB'
014600041005C1790C* RID
014700041005C1790C     TIPREC        OREQ      'IR'
014800000000     C                   MOVEL     RECORD        NDCKRBRB
014900000000     C* Inizializzazioni nuova contabile
015000000000     C                   Clear                   SAVUnita
015100000000     C                   Move      *Off          $PrimoEff
015200000000     C                   Move      *Off          $MultiUni
015300000000     C                   Move      *Off          $InsoCont
015400000000     C                   Move      *Off          $SocietaOK
015401140103>>>>>C                   IF        rbtabi = xscSia
015402140103>>>>>C                   EVAL      $SocietaOK = *ON
015403140103>>>>>C                   ENDIF
015500041005C1790C                   Move      *Off          $Rid              1
015600000000     C                   Move      *Blank        FLPRIM            1
015700041005C1790C     TIPREC        IFEQ      'IR'
015800000000"    C                   Move      *On           $Rid
015900041005C1790C                   ENDIF
016000000000     C                   GOTO      END
016100000000     C                   ENDIF
016200000000     C*
016300000000     C*
016400041005C1790C*    TIPREC        IFEQ      '14'
016500041005  "  C*    FLPRIM        ANDEQ     *BLANK
016600041005  "  C                   IF        (TIPREC     = '14'
016700041005  "  C                             AND FLPRIM = *BLANK)
016800041005  "  C                             OR (TIPREC     = '10'
016900041005C1790C                             AND $rid   = *on)
017000000000     C                   MOVEL     RECORD        NDCKRB14
017001140102>>>>>C                   IF        rbaazn = *BLANK
017002140102>>>>>C                   EVAL      rbaazn = rbtabi
017003140102>>>>>C                   ENDIF
017100041005C1790C                   IF        FLPRIM  =  *blank
017200000000     C* Controllo per lettura contabili solo della società corrente
017300000000     C* (controllo SIA su rcd 14 e non su rcd RB perchè ci sono situazioni
017400000000     C*  multisocietarie in cui su rcd RB c'è sempre il SIA della capofila
017500000000     C*  e non quello della società a cui si riferisce l'effetto insoluto)
017600000000     C                   If        RBAAZN = XScSIA
017700000000     C                   Move      *On           $SocietaOK        1
017800000000     C* Reperisco nr. progressivo di carico
017900000000     C                   Exsr      RepPrgCar
018000000000     C                   EndIf
018100000000     C* Controlli solo per il primo rcd "14"
018200000000     C                   EXSR      SRPRIM
018300000000     C                   ENDIF
018400041005C1790C                   ENDIF
018500000000     C*
018600000000     C* Se la società non è OK salto tutti i record della contabile
018700000000     C                   If        $SocietaOK = *Off
018800000000     C                   GOTO      END
018900000000     C                   EndIf
019000000000     C*
019100000000     C     TIPREC        IFEQ      'EF'
019200000000     C                   EXCEPT    PRT0EF
019300000000     C* Fine carico
019400000000     C                   EXSR      ENDCAR
019500000000     C                   GOTO      END
019600000000     C                   ENDIF
019700000000     C*
019800041005C1790C     TIPREC        IFEQ      '10'
019900041005  "  C*    $Rid          ANDEQ     *Off
020000041005  "  C                   if        $rid   = *Off
020100041005  "  C                             or ($rid   = *On
020200041005C1790C                             and rbcaus = 16000)
020300000000     C                   MOVEL     RECORD        NDCKRB10
020400000000     C                   EXSR      SBR010
020500000000     C                   GOTO      END
020600041005C1790C                   ENDIF
020700000000     C                   ENDIF
020800000000     C*
020900000000     C     TIPREC        IFEQ      '40'
021000000000     C                   MOVEL     RECORD        NDCKRB40
021100000000     C                   MOVE      RBDLOC        PRTPRV            2
021200000000     C                   GOTO      END
021300000000     C                   ENDIF
021400000000     C*
021500041005C1790C     TIPREC        IFEQ      '20'
021600000000"    C     $Rid          ANDEQ     *On
021700000000"    C                   SETON                                        20
021800000000"    C                   MOVEL     RECORD        NDRIBA20DS
021900041005C1790C                   ENDIF
022000000000     C*
022100041005C1790C     TIPREC        IFEQ      '70'
022200000000"    C     $Rid          ANDEQ     *On
022300000000"    C                   SETON                                        70
022400000000"    C                   MOVEL     RECORD        NDRIBA70DS
022500041005C1790C                   ENDIF
022600000000     C*
022700000000     C     TIPREC        IFEQ      '30'
022800000000     C                   SETON                                        30
022900041005C1790C     $Rid          IFEQ      *Off
023000000000     C                   MOVEL     RECORD        NDCKRB30
023100041005C1790C                   ELSE
023200000000"    C                   MOVEL     RECORD        NDRIBA30DS
023300041005C1790C                   ENDIF
023400000000     C                   ELSE
023500000000     C*
023600000000     C     TIPREC        IFEQ      '50'
023700000000     C                   SETON                                        50
023800041005C1790C     $Rid          IFEQ      *Off
023900000000     C                   MOVEL     RECORD        NDCKRB50
024000041005C1790C                   ELSE
024100000000"    C                   MOVEL     RECORD        NDRIBA50DS
024200041005C1790C                   ENDIF
024300000000     C                   ELSE
024400000000     C*
024500000000     C     TIPREC        IFEQ      '51'
024600000000     C                   SETON                                        51
024700000000     C                   MOVEL     RECORD        NDCKRB51
024800131223>>>>>C                   IF        %SUBST(NDCKRB51:66:6) = *BLANK
024900131223>>>>>C                   CLEAR                   RBAUTD
025000131223>>>>>C                   ENDIF
025100131223>>>>>C                   IF        %SUBST(NDCKRB51:72:15) = *BLANK
025200131223>>>>>C                   CLEAR                   RBPORN
025300131223>>>>>C                   ENDIF
025400131223>>>>>C                   IF        %SUBST(NDCKRB51:87:5) = *BLANK
025500131223>>>>>C                   CLEAR                   RBSP51
025600131223>>>>>C                   ENDIF
025700131223>>>>>C                   IF        %SUBST(NDCKRB51:92:6) = *BLANK
025800131223>>>>>C                   CLEAR                   RBDV51
025900131223>>>>>C                   ENDIF
026000000000     C                   MOVE      RBOC51        RDKOC51
026100000000     C                   ELSE
026200000000     C*
026300041005C1790C**   TIPREC        IFNE      '14'
026400041005 "   C                   IF        (TIPREC <> '14' AND $Rid = *off)
026500041005 "   C                             or (TIPREC <> '10' AND $Rid = *on
026600041005C1790C                                 AND RBCAUS  <> 16000)
026700000000     C                   Z-ADD     0             RBIMPO
026800000000     C                   Z-ADD     5             RC
026900000000     C                   EXSR      SRSUM
027000000000     C                   EXCEPT    BADSEQ
027100000000     C                   GOTO      END
027200000000     C                   ELSE
027300000000     C*
027400000000     C                   SETON                                        14
027500000000     C                   SETOFF                                       10
027600000000     C                   SETOFF                                       305051
027700000000     C                   SETOFF                                       202122
027800000000     C                   SETOFF                                       2324
027900000000     C                   MOVE      *BLANK        FLG
028000000000     C                   MOVEL     RECORD        NDCKRB14
028100000000     C                   EXSR      VALCAM
028200000000     C*
028300000000     C                   ENDIF
028400000000     C                   ENDIF
028500000000     C                   ENDIF
028600000000     C                   ENDIF
028700000000      *
028800041005C1790C                   IF        $Rid = *On
028900000000"    C                             and *IN50 = *on
029000000000"    C                   EXSR      ROTRID
029100000000"    C                   EXCEPT    PRTERR
029200000000"    C                   SETOFF                                       50
029300041005C1790C                   ENDIF
029400000000     C*
029500000000     C   51              EXSR      ROTEFF
029600000000     C   51              EXCEPT    PRTERR
029700000000     C                   SETOFF                                       51
029800000000      *
029900000000     C     END           TAG
030000000000      *
030100000000      * Cancello il record solo se è stato "trasferito"
030200000000     C     $SocietaOK    IfEq      *On
030300000000     C                   DELETE    NDRIK00F
030400000000     C                   EndIf
030500000000      *
030600000000     CLR                 EXSR      ENDPGM
030700000000     CLR   FINE          TAG
030800000000      *
030900000000      *----------------------------------------------------------------
031000000000      * CONTROLLO RECORD DETTAGLIO
031100000000      *----------------------------------------------------------------
031200000000     C     ROTEFF        BEGSR
031300000000      *
031400000000     C                   Z-ADD     RTKCARN       RDKDS14
031500000000     C                   Z-ADD     RBRICN        RBDSOR
031600000000     C                   Z-ADD     RBRICN        RDKDSOR
031700000000     C     *DMY          TEST(D)                 RBDSCA                 99
031800000000     C  N99*DMY          Move      RBDSCA        Data10
031900000000     C   99              Move      *Loval        Data10
032000000000     C   99              Z-Add     0             RBDSCA
032100000000     C                   Move      Data10        RDKDSCA
032200000000     C     K04RDK01      CHAIN     NDRDK01L                           08
032300000000      *
032400000000      * NON CARICO INSOLUTO GIA' CARICATO
032500000000     C     *IN08         IFEQ      *ON
032600000000     C* Effetto originario
032700000000     C                   MOVE      'CG'          EFACTB
032800000000     C                   Z-ADD     RBRICN        EFANREFF
032900000000     C     K03EFA02      CHAIN(N)  NDEFA02L                           09
033000000000     C   09              CLEAR                   NDEFA000
033100000000D0650C   09              Move      *On           $NoEFA
033200000000D0650C  N09              Move      *Off          $NoEFA
033300090128D2474C* se trova NDEFA reperisce banca
033400090128  "  c                   if        *in09=*off
033500090128  "  C                   exsr      RepBanca
033600090128  "  c                   else
033700090128  "  c                   clear                   RTKBKCC
033800090128  "  C                   clear                   RTKBKSC
033900090128D2474C                   endif
034000000000     C* Unità dell'effetto originario
034100000000     C                   Z-Add     EfaSys        RegSys
034200000000     C                   Z-Add     EfaNrAsREg    RegNrAsReg
034300000000     C     K02REG01      CHAIN     NDREG01L                           09
034400000000     C   09              CLEAR                   NDREG000
034500000000     C                   MOVE      REGUNITA      RDKUNITA
034600000000     C* Se unità diverse per la stessa contabile lo segnalo
034700000000     C                   If        $PrimoEff <> *On
034800000000     C                   Move      *On           $PrimoEff         1
034900000000     C                   Move      REGUnita      SAVUnita
035000000000     C                   Else
035100000000     C                   If        REGUnita <> SAVUnita
035200000000     C                   Move      *On           $MultiUni         1
035300000000     C                   EndIf
035400000000     C                   EndIf
035500000000     C* TIPO ESITO
035600000000     C                   Z-ADD     0             RC
035700000000     C     RBCAUS        IFNE      *ZERO
035800000000     C                   MOVE      XSCCUT        XTBUTE
035900000000     C                   MOVE      '1'           XTBRIC
036000000000     C                   MOVE      XSCSOC        XTBAZI
036100000000     C                   MOVE      *ZERO         XTBKEY
036200000000     C                   MOVE      'I07'         XTBCOD
036300000000     C                   MOVE      RBCAUS        XTBKEY
036400000000     C                   CALLb     'XATB'
036500000000     C                   PARM                    XATBDS
036600000000     C     XTBERR        IFEQ      *ON
036700050112D1563c                   EXCEPT    PRTERR1
036800000000     C                   Z-ADD     4             RC
036900000000     C                   ELSE
037000000000     C                   MOVEL     XTBUNI        TABI07
037100000000     C                   MOVEL     ESII07        FLG(4)
037200000000     C     ESII07        IFEQ      'P'
037300000000     C                   Z-ADD     1             RC
037400000000     C                   ELSE
037500000000     C     ESII07        IFEQ      'S'
037600000000     C                   Z-ADD     2             RC
037700000000     C                   ELSE
037800000000     C     ESII07        IFEQ      'I'
037900000000     C                   Z-ADD     3             RC
038000000000     C                   ELSE
038100000000     C                   Z-ADD     4             RC
038200000000     C                   ENDIF
038300000000     C                   ENDIF
038400000000     C                   ENDIF
038500000000     C                   ENDIF
038600000000     C*
038700000000     C                   SETOFF                                       202125
038800000000     C  N22RC            IFGT      1
038900000000     C     EfaEsito      ANDEQ     '4'
039000000000     C                   SETON                                        25
039100000000     C                   MOVE      '*'           FLG(4)
039200000000     C                   ENDIF
039300000000     C*
039400000000D0650C                   If        $NoEFA = *On
039500000000  "  C                   SetOn                                        29
039600000000  "  C                   Else
039700000000D0650C                   SetOff                                       29
039800000000     C  N25
039900000000     CANN22
040000000000     CANN23
040100000000     CANN24              SETON                                        20
040200000000D0650C                   EndIf
040300000000     C*
040400000000     C  N20              SETON                                        21
040500000000     C   22              MOVE      ' *'          FLG(1)
040600000000     C   23              MOVE      ' *'          FLG(2)
040700000000     C   24              MOVE      '* '          FLG(3)
040800000000     C*
040900000000     C   21
041000000000     CANN22              DO
041100000000     C                   CALLb     'XDT4000'
041200000000     C                   PARM      EfaDtSca      XDTAMG           10
041300000000     C     EFWDSC        PARM                    XDTGMA            6 0
041400000000     C                   PARM      2             XDTSTA            1 0
041500000000     C                   CALLb     'XDT4000'
041600000000     C                   PARM      EfaDtPar      XDTAMG           10
041700000000     C     EFWDRP        PARM                    XDTGMA            6 0
041800000000     C                   PARM      2             XDTSTA            1 0
041900000000     C                   END
042000000000D0638C                   Z-Add     EFAImportD    EFWIMD           14 3
042100000000     C*
042200000000     C                   EXSR      SRSUM
042300000000     C     *DMY          TEST(D)                 RBDV51                 99
042400000000     C  N99*DMY          MOVE      RBDV51        Data10
042500000000     C   99              MOVE      *LOVAL        Data10
042600000000     C   99              Z-ADD     0             RBDV51
042700000000     C                   MOVE      Data10        RDKVSIA
042800000000      * Se codice divisa è 'E' (euro) le ultime 2 posizioni dell'importo
042900000000      *  sono i decimali (quindi divido l'importo per 100)
043000080122D2335C                   If        Spese51 =  *blanks
043100080122D2335C                   Z-Add     0             RBSP51
043200080122D2335C                   EndIf
043300000000     C                   Z-Add     RBSP51        WWImporto
043400000000     C                   If        RBTCDD = 'E'
043500000000     C                   DIV       100           WWImporto
043600000000     C                   EndIf
043700000000     C                   Z-Add     WWImporto     RDKIMSP
043800000000     C                   ADD       RDKIMSP       RTKSBIS
043900000000     C                   MOVE      *LOVAL        Data10
044000000000     C                   MOVE      Data10        RDKVMED
044100000000     C*
044200000000     C     RC            IFEQ      3
044300000000     C                   Move      *On           $InsoCont         1
044400000000     C                   WRITE     NDRDK000
044500000000     C                   ENDIF
044600000000     C*
044700000000     C* Aggiornamento esito "pagato" e data esito sull'effetto
044800000000     C     RC            IFEQ      1
044900000000     C                   MOVE      'CG'          EFACTB
045000000000     C                   Z-ADD     RBRICN        EFANREFF
045100000000     C     K03EFA02      CHAIN     NDEFA02L                           19
045200000000     C                   If        *In19 = *Off
045300000000     C                   Move      '3'           EFAEsito
045400000000     C                   Move      Oggi          EFADtEsito
045500000000     C                   UPDATE    NDEFA000
045600000000     C                   EndIf
045700000000     C                   ENDIF
045800000000     C*
045900000000     C                   ENDIF
046000000000     C*
046100000000     C                   ENDIF
046200000000     C*
046300000000     C                   ENDSR
046400041005C1790 *----------------------------------------------------------------
046500000000"     * CONTROLLO RECORD DETTAGLIO RID
046600000000"     *----------------------------------------------------------------
046700041005C1790C     ROTRID        BEGSR
046800000000      *
046900000000     C                   Z-ADD     RTKCARN       RDKDS14
047000000000      *
047100000000     C* Se nel tipo record 50 il numero RID è significativo
047200000000     C* valorizzo RBRICN per leggere NDEFA02L
047300000000   1bC     DSNRRID       IFNE      *blank
047400000000     C                   Move      DSNRRID       NRRID             9 0
047500000000     C                   Z-ADD     NRRID         RBRICN
047600000000   1xC                   ELSE
047700000000     C* leggo NDEFA03L per ricavare il numero RID
047800000000     C                   EXSR      REDEFA03
047900000000   2bC     $EFAOK        IFEQ      *on
048000000000     C                   Z-ADD     NRRID         RBRICN
048100000000      *
048200000000   2xC                   ELSE
048300000000     C                   Z-ADD     0             RBRICN
048400000000   2eC                   ENDIF
048500000000   1eC                   ENDIF
048600000000      *
048700000000     C                   Z-ADD     RBRICN        RBDSOR
048800000000     C                   Z-ADD     RBRICN        RDKDSOR
048900000000     C     *DMY          TEST(D)                 RBDSCA                 99
049000000000     C  N99*DMY          Move      RBDSCA        Data10
049100000000     C   99              Move      *Loval        Data10
049200000000     C   99              Z-Add     0             RBDSCA
049300000000     C                   Move      Data10        RDKDSCA
049400000000     C     K04RDK01      CHAIN     NDRDK01L                           08
049500000000      *
049600000000      * NON CARICO INSOLUTO GIA' CARICATO
049700000000     C     *IN08         IFEQ      *ON
049800000000     C* Effetto originario
049900000000     C                   MOVE      'CG'          EFACTB
050000000000     C                   Z-ADD     RBRICN        EFANREFF
050100000000     C     K03EFA02      CHAIN(N)  NDEFA02L                           09
050200000000     C   09              CLEAR                   NDEFA000
050300000000D0650C   09              Move      *On           $NoEFA
050400000000D0650C  N09              Move      *Off          $NoEFA
050500090128D2474C* se trova NDEFA reperisce banca
050600090128  "  c                   if        *in09=*off
050700090128  "  C                   exsr      RepBanca
050800090128  "  c                   else
050900090128  "  c                   clear                   RTKBKCC
051000090128  "  C                   clear                   RTKBKSC
051100090128D2474C                   endif
051200000000     C* Unità dell'effetto originario
051300000000     C                   Z-Add     EfaSys        RegSys
051400000000     C                   Z-Add     EfaNrAsREg    RegNrAsReg
051500000000     C     K02REG01      CHAIN     NDREG01L                           09
051600000000     C   09              CLEAR                   NDREG000
051700000000     C                   MOVE      REGUNITA      RDKUNITA
051800000000     C* Se unità diverse per la stessa contabile lo segnalo
051900000000     C                   If        $PrimoEff <> *On
052000000000     C                   Move      *On           $PrimoEff         1
052100000000     C                   Move      REGUnita      SAVUnita
052200000000     C                   Else
052300000000     C                   If        REGUnita <> SAVUnita
052400000000     C                   Move      *On           $MultiUni         1
052500000000     C                   EndIf
052600000000     C                   EndIf
052700000000     C* TIPO ESITO
052800000000     C                   Z-ADD     0             RC
052900000000     C     RBCAUS        IFNE      *ZERO
053000000000     C                   MOVE      XSCCUT        XTBUTE
053100000000     C                   MOVE      '1'           XTBRIC
053200000000     C                   MOVE      XSCSOC        XTBAZI
053300000000     C                   MOVE      *ZERO         XTBKEY
053400000000     C                   MOVE      'I07'         XTBCOD
053500000000     C                   MOVE      RBCAUS        XTBKEY
053600000000     C                   CALLb     'XATB'
053700000000     C                   PARM                    XATBDS
053800000000     C     XTBERR        IFEQ      *ON
053900000000     C                   Z-ADD     4             RC
054000000000     C                   ELSE
054100000000     C                   MOVEL     XTBUNI        TABI07
054200000000     C                   MOVEL     ESII07        FLG(4)
054300000000     C     ESII07        IFEQ      'P'
054400000000     C                   Z-ADD     1             RC
054500000000     C                   ELSE
054600000000     C     ESII07        IFEQ      'S'
054700000000     C                   Z-ADD     2             RC
054800000000     C                   ELSE
054900000000     C     ESII07        IFEQ      'I'
055000000000     C                   Z-ADD     3             RC
055100000000     C                   ELSE
055200000000     C                   Z-ADD     4             RC
055300000000     C                   ENDIF
055400000000     C                   ENDIF
055500000000     C                   ENDIF
055600000000     C                   ENDIF
055700000000     C*
055800000000     C                   SETOFF                                       202125
055900000000     C  N22RC            IFGT      1
056000000000     C     EfaEsito      ANDEQ     '4'
056100000000     C                   SETON                                        25
056200000000     C                   MOVE      '*'           FLG(4)
056300000000     C                   ENDIF
056400000000     C*
056500000000D0650C                   If        $NoEFA = *On
056600000000  "  C                   SetOn                                        29
056700000000  "  C                   Else
056800000000D0650C                   SetOff                                       29
056900000000     C  N25
057000000000     CANN22
057100000000     CANN23
057200000000     CANN24              SETON                                        20
057300000000D0650C                   EndIf
057400000000     C*
057500000000     C  N20              SETON                                        21
057600000000     C   22              MOVE      ' *'          FLG(1)
057700000000     C   23              MOVE      ' *'          FLG(2)
057800000000     C   24              MOVE      '* '          FLG(3)
057900000000     C*
058000000000     C   21
058100000000     CANN22              DO
058200000000     C                   CALLb     'XDT4000'
058300000000     C                   PARM      EfaDtSca      XDTAMG           10
058400000000     C     EFWDSC        PARM                    XDTGMA            6 0
058500000000     C                   PARM      2             XDTSTA            1 0
058600000000     C                   CALLb     'XDT4000'
058700000000     C                   PARM      EfaDtPar      XDTAMG           10
058800000000     C     EFWDRP        PARM                    XDTGMA            6 0
058900000000     C                   PARM      2             XDTSTA            1 0
059000000000     C                   END
059100000000D0638C                   Z-Add     EFAImportD    EFWIMD           14 3
059200000000     C*
059300000000     C                   EXSR      SRSUM
059400041005C1790C*    *DMY          TEST(D)                 RBDV51                 99
059500041005  "  C* N99*DMY          MOVE      RBDV51        Data10
059600041005  "  C*  99              MOVE      *LOVAL        Data10
059700041005  "  C*  99              Z-ADD     0             RBDV51
059800041005C1790C*                  MOVE      Data10        RDKVSIA
059900000000      * Se codice divisa è 'E' (euro) le ultime 2 posizioni dell'importo
060000000000      *  sono i decimali (quindi divido l'importo per 100)
060100131223C1790C*                  Z-Add     DSIMPSPESE    WWImporto
060200041005  "  C*                  If        RBTCDD = 'E'
060300041005  "  C                   DIV       100           WWImporto
060400041005  "  C*                  EndIf
060500041005  "  C                   Z-Add     WWImporto     RDKIMSP
060600041005C1790C                   ADD       RDKIMSP       RTKSBIS
060700000000     C                   MOVE      *LOVAL        Data10
060800000000     C                   MOVE      Data10        RDKVMED
060900000000     C*
061000000000     C     RC            IFEQ      3
061100000000     C                   Move      *On           $InsoCont         1
061200000000     C                   WRITE     NDRDK000
061300000000     C                   ENDIF
061400000000     C*
061500000000     C* Aggiornamento esito "pagato" e data esito sull'effetto
061600000000     C     RC            IFEQ      1
061700000000     C                   MOVE      'CG'          EFACTB
061800000000     C                   Z-ADD     RBRICN        EFANREFF
061900000000     C     K03EFA02      CHAIN     NDEFA02L                           19
062000000000     C                   If        *In19 = *Off
062100000000     C                   Move      '3'           EFAEsito
062200000000     C                   Move      Oggi          EFADtEsito
062300000000     C                   UPDATE    NDEFA000
062400000000     C                   EndIf
062500000000     C                   ENDIF
062600000000     C*
062700000000     C                   ENDIF
062800000000     C*
062900000000     C                   ENDIF
063000000000     C*
063100041005C1790C                   ENDSR
063200041005C1790C*----------------------------------------------------------------
063300000000     C* LETURA NDEFA03L
063400000000     C*----------------------------------------------------------------
063500041005C1790C     REDEFA03      BEGSR
063600000000     C*
063700000000     C                   MOVE      *Off          $EFAok            1
063800000000     C*
063900000000     C                   MOVE      DSNRFATT      NRFATT            9 0
064000000000     C*
064100000000     C                   CLEAR                   XDATCDS
064200000000     C                   MOVEL     DSDTFATT      XDADT1
064300000000     C                   MOVEL     '*EUR'        XDAFM1
064400000000     C                   MOVEL     '*ISO'        XDAFM2
064500000000     C                   MOVEL     '*NONE'       XDASEP
064600000000     C                   CALL      'XDATC'
064700000000     C                   PARM                    XDATCDS
064800000000     C     XDADT2        IFNE      'ERRORE'
064900000000     C                   MOVEL     XDADT2        DTFATT10         10
065000000000     C                   ELSE
065100000000     C                   MOVEL     *loval        DTFATT10
065200000000     C                   ENDIF
065300000000     C*
065400000000     C                   MOVE      'CG'          EFACTB
065500000000     C                   MOVE      RBAKSC        EFAKSC
065600000000     C*
065700000000     C     *DMY          TEST(D)                 RBDSCA                 99
065800000000     C  N99*DMY          Move      RBDSCA        Data10
065900000000     C   99              Move      *Loval        Data10
066000000000     C   99              Z-Add     0             RBDSCA
066100000000     C                   Move      Data10        RBDSCA10         10
066200000000     C*
066300000000     C     K04EFA03      SETLL     NDEFA03L
066400000000     C     K04EFA03      READE(N)  NDEFA03L                               26
066500000000     C                   MOVE      *In26         $EFA03            1
066600000000   1bC     $EFA03        DOWEQ     *OFF
066700000000   1bC     $EFAok        ANDEQ     *OFF
066800041118     C* vedo se effetto RID
066900041118     C                   Reset                   xatbds
067000041118     C                   eval      xtbric        = '1'
067100041118     C                   eval      xtbazi        = xscsoc
067200041118     C                   eval      xtbcod        = '0E1'
067300041118     C                   eval      xtbkey        = *zeros
067400041118     C                   move      EfaMdPag      xtbkey
067500041118     C                   CALLb     'XATB'
067600041118     C                   PARM                    XATBDS
067700041118 b2  C     XTBERR        IFEQ      '0'
067800041118     C                   MOVEL     XTBUNI        Tab0e1
067900041118    bC     Sm00E1        IFEQ      'R'
068000000000     C     EFANRPAR      ANDEQ     NRFATT
068100000000     C     EFADTPAR      ANDEQ     DTFATT10
068200000000     C     EFAIMPORTO    ANDEQ     Rdkimpo
068300000000     C                   Z-ADD     EFANREFF      NRRID
068400000000     C                   MOVE      *On           $EFAok
068500000000     C                   LEAVE
068600000000   2eC                   ENDIF
068700041118   1eC                   ENDIF
068800000000     C*
068900000000     C     K04EFA03      READE(N)  NDEFA03L                               26
069000000000     C                   MOVE      *In26         $EFA03
069100000000   1eC                   ENDDO
069200000000     C*
069300041005C1790C                   ENDSR
069400000000     C*----------------------------------------------------------------
069500000000     C* VALORIZZAZIONE CAMPI FILE NDRDK DA TR."14"
069600000000     C*----------------------------------------------------------------
069700000000     C     VALCAM        BEGSR
069800000000     C*
069900000000     C                   MOVE      RBBA14        RDKBA14
070000000000     C                   MOVE      RBTR14        RDKTR14
070100000000     C     *DMY          TEST(D)                 RBDCRE                 99
070200000000     C  N99*DMY          MOVE      RBDCRE        Data10
070300000000     C   99              MOVE      *LOVAL        Data10
070400000000     C   99              Z-ADD     0             RBDCRE
070500000000     C                   MOVE      Data10        RDKDCRE
070600000000     C     *DMY          TEST(D)                 RBVSIA                 99
070700000000     C  N99*DMY          MOVE      RBVSIA        Data10
070800000000     C   99              MOVE      *LOVAL        Data10
070900000000     C   99              Z-ADD     0             RBVSIA
071000000000     C                   MOVE      Data10        RDKVSIA
071100000000     C     *DMY          TEST(D)                 RBDSCA                 99
071200000000     C  N99*DMY          MOVE      RBDSCA        Data10
071300000000     C   99              MOVE      *LOVAL        Data10
071400000000     C   99              Z-ADD     0             RBDSCA
071500000000     C                   MOVE      Data10        RDKDSCA
071600041005C1790c                   if        $rid = *ON
071700041005  "  C                   MOVE      Data10        RDKVSIA
071800041005C1790c                   endif
071900000000     C                   Z-ADD     RBCAUS        RDKCAUS
072000000000      * Se codice divisa è 'E' (euro) le ultime 2 posizioni dell'importo
072100000000      *  sono i decimali (quindi divido l'importo per 100)
072200000000     C                   Z-Add     RBIMPO        WWImporto
072300000000     C                   If        RBTCDD = 'E'
072400000000     C                   DIV       100           WWImporto
072500000000     C                   EndIf
072600000000     C                   Z-Add     WWImporto     RDKIMPO
072700000000     C                   MOVE      RBSEGN        RDKSEGN
072800000000     C                   MOVE      RBOABI        RDKOABI
072900000000     C                   MOVE      RBOCAB        RDKOCAB
073000000000     C                   MOVEL     RBOCOC        RDKOCOC
073100000000     C                   MOVE      RBDABI        RDKDABI
073200000000     C                   MOVE      RBDCAB        RDKDCAB
073300000000     C                   MOVEL     RBDCOC        RDKDCOC
073400000000     C                   MOVE      RBAAZN        RDKAAZN
073500000000     C                   MOVE      RBATCO        RDKATCO
073600000000     C                   MOVE      RBAKCC        RDKAKCC
073700000000     C                   MOVE      RBAKSC        RDKAKSC
073800000000     C*
073900000000D0638C                   Z-Add     RDKImpo       RSIMPO           14 3
074000000000     C*
074100000000     C                   ENDSR
074200000000     C*----------------------------------------------------------------
074300000000     C* GESTIONE RIEPILOGHI
074400000000     C*----------------------------------------------------------------
074500000000     C     SRSUM         BEGSR
074600000000     C*
074700000000     C     RC            OCCUR     RIEPIL
074800000000     C     DFL           IFEQ      *BLANK
074900000000     C                   MOVE      *ZERO         DFL
075000000000     C                   MOVE      *ZERO         DNU
075100000000     C                   MOVE      *ZERO         DIM
075200000000     C                   ENDIF
075300000000     C   20              Z-ADD     1             D0
075400000000     C  N20              Z-ADD     2             D0
075500000000     C  N90              ADD       1             DNU(D0)
075600000000     C  N90              DO
075700000000      * Se codice divisa è 'E' (euro) le ultime 2 posizioni dell'importo
075800000000      *  sono i decimali (quindi divido l'importo per 100)
075900000000     C                   Z-Add     RBIMPO        WWImporto
076000000000     C                   If        RBTCDD = 'E'
076100000000     C                   DIV       100           WWImporto
076200000000     C                   EndIf
076300000000     C                   ADD       WWImporto     DIM(D0)
076400000000     C                   END
076500000000     C*
076600000000     C                   ENDSR
076700000000     C*----------------------------------------------------------------
076800000000     C* GESTIONE STAMPA
076900000000     C*----------------------------------------------------------------
077000000000     C     SRPRT         BEGSR
077100000000     C*
077200000000     C                   SETON                                        90
077300000000     C                   EXCEPT    HDRTOT
077400000000     C                   DO        6             RC
077500000000     C                   MOVEL     'IMPORTI'     CSTIMP            8
077600000000     C     RC            IFLT      6
077700000000     C     RC            OCCUR     RIEPIL
077800000000     C     DFL           CASEQ     *BLANK        SRSUM
077900000000     C                   END
078000000000     C                   XFOOT     DNU           DNU(3)
078100000000     C                   XFOOT     DIM           DIM(3)
078200000000     C                   ADD       DNU           TNU
078300000000     C                   ADD       DIM           TIM
078400000000     C     RC            IFEQ      1
078500000000     C                   Z-ADD     DNU(3)        RTKPAGN
078600000000     C                   Z-ADD     DIM(3)        RTKPAGI
078700000000     C                   ENDIF
078800000000     C     RC            IFEQ      2
078900000000     C                   Z-ADD     DNU(3)        RTKSTON
079000000000     C                   Z-ADD     DIM(3)        RTKSTOI
079100000000     C                   ENDIF
079200000000     C     RC            IFEQ      3
079300000000     C                   Z-ADD     DNU(3)        RTKINSN
079400000000     C                   Z-ADD     DIM(3)        RTKINSI
079500000000     C                   ENDIF
079600000000     C     RC            IFEQ      4
079700000000     C                   Z-ADD     DNU(3)        RTKALTN
079800000000     C                   Z-ADD     DIM(3)        RTKALTI
079900000000     C                   ENDIF
080000000000     C     RC            IFEQ      5
080100000000     C                   MOVEL     *BLANKS       CSTIMP
080200000000     C                   ENDIF
080300000000     C                   ELSE
080400000000     C                   Z-ADD     TIM           DIM
080500000000     C                   Z-ADD     TNU           DNU
080600000000     C                   ENDIF
080700000000     C                   EXCEPT    TOTAL
080800000000     C                   ENDDO
080900000000     C*
081000000000     C                   ENDSR
081100000000     C*----------------------------------------------------------------
081200000000     C* CONTROLLO CODICE BANCA AL PRIMO RECORD "14"
081300000000     C*----------------------------------------------------------------
081400000000     C     SRPRIM        BEGSR
081500000000     C*
081600000000     C                   MOVE      '1'           FLPRIM
081700000000     C*
081800000000     C     RBAAZN        IFNE      XSCSIA
081900000000     C     RBAAZN        ANDNE     SAVSiaNoOk
082000000000     C* (emetto window solo prima volta che trovo un cod.SIA non corretto)
082100000000     C                   Eval      SAVSiaNoOk = RBAAZN
082200000000     C*
082300000000     C                   CLEAR                   XMSGDS
082400000000     C* Txt messaggio
082500000000     C                   MOVEL     $COS(1)       IDMSG
082600000000     C                   CALLB     'XRTVM'
082700000000     C                   PARM                    IDMSG            27
082800000000     C                   PARM                    TXTMSG          100
082900000000     C                   PARM                    ERRMSG            1
083000000000     C     ERRMSG        IFNE      *ON
083100000000     C                   MOVEL     TXTMSG        MSGMS2
083200000000     C                   CAT       RBAAZN:1      MSGMS2
083300000000     C                   ELSE
083400000000     C                   MOVEL     *ALL'?'       MSGMS2
083500000000     C                   ENDIF
083600000000     C* Txt bottoni risposta
083700000000     C                   MOVEL     TxtBott       IDMSG
083800000000     C                   CALLB     'XRTVM'
083900000000     C                   PARM                    IDMSG            27
084000000000     C                   PARM                    TXTMSG          100
084100000000     C                   PARM                    ERRMSG            1
084200000000     C                   PARM      *OFF          CTRMSG            1
084300000000     C                   PARM      135           LENMSG            3 0
084400000000     C     ERRMSG        IFNE      *ON
084500000000     C                   MOVEL     TXTMSG        XMSGTXT
084600000000     C                   ELSE
084700000000     C                   MOVEL     *ALL'?'       XMSGTXT
084800000000     C                   ENDIF
084900000000     C*
085000000000     C                   Z-ADD     0             MSGMSG
085100000000     C                   MOVEL     'S'           MSGRSP
085200000000     C                   MOVEL     'AC'          MSGTPR
085300000000     C                   MOVEL     'S'           MSGEMV
085400000000     C                   Z-ADD     10            MSGRGP
085500000000     C                   MOVEL     'N'           MSGLCK
085600000000     C                   MOVEA     STATUS        STS
085700000000     C                   MOVEL     'N'           MSGCNL
085800000000     C                   MOVEL     'N'           MSGVID
085900000000     C                   MOVEL     'N'           MSGOPE
086000000000     C                   MOVEL     'N'           MSGSTP
086100000000     C*
086200000000     C                   CALL      'XMSGR'
086300000000     C                   PARM                    XMSGDS
086400000000     C                   PARM                    XMSGTXT         135
086500000000     C                   IF        MSGRTN = 'A'
086600000000     C                   SETON                                        LR
086700000000     C                   GOTO      FINE
086800000000     C                   ENDIF
086900000000     C                   ENDIF
087000000000     C*
087100000000     C* PROSEGUIMENTO ELABORAZIONE DEFINITIVA
087200000000     C* (verranno scartate comunque le contabili che non appartengono
087300000000     C*  alla società corrente !!!)
087400000000     C                   Z-ADD     PrgCarico     RTKCARN
087500000000     C                   TIME                    RTKCART
087600000000     C                   MOVE      Oggi          RTKCARD
087700000000     C     *DMY          TEST(D)                 RBTCRE                 99
087800000000     C  N99*DMY          Move      RBTCRE        Data10
087900000000     C   99              Move      *Loval        Data10
088000000000     C   99              Z-ADD     0             RBTCRE
088100000000     C                   Move      Data10        RTKMOVD
088200000000     C     RBTAZN        IFNE      XSCSIA
088300000000     C                   MOVE      RBTAZN        RTKOABI
088400000000     C                   MOVE      RBTABI        RTKAAZN
088500000000     C                   ELSE
088600000000     C                   MOVE      RBTABI        RTKOABI
088700000000     C                   MOVE      RBTAZN        RTKAAZN
088800000000     C                   ENDIF
088900000000     C                   MOVE      *ZERO         RTKOCAB
089000000000     C                   MOVE      RBOCOC        RTKOCOC
089100000000     C                   MOVE      *LOVAL        Data10
089200000000     C                   MOVE      Data10        RTKOPED
089300000000     C                   MOVE      Data10        RTKVALD
089400000000     C*
089500000000     C                   ENDSR
089600000000     C*------------------------------------------------------------------*
089700000000     C* OPERAZIONI PER TIPO RECORD "10" = PROMEMORIA CONTABILE
089800000000     C*------------------------------------------------------------------*
089900000000     C     SBR010        BEGSR
090000000000      *
090100000000      * Data valuta media (aggiornamento su tutti i record della contabile)
090200000000      *
090300000000     C     RBCA10        IFEQ      37000
090400000000     C     *DMY          Test(D)                 RBVL10                 99
090500000000     C                   If        *In99 = *Off
090600000000     C     *DMY          Move      RBVL10        DataValM
090700000000     C                   Call      'NDCK38R'
090800000000     C                   Parm                    RDKSOCIETA
090900000000     C                   Parm                    RDKDS14
091000000000     C                   Parm                    RDKOC51
091100000000     C                   Parm                    DataValM
091200000000     C                   Parm                    $MultiUni
091300000000     C                   EndIf
091400000000     C                   ENDIF
091500000000      *
091600000000      * Totale spese e commissioni
091700000000      *
091800000000     C     RBCA10        IFEQ      16000
091900000000      * Se codice divisa è 'E' (euro) le ultime 2 posizioni dell'importo
092000000000      *  sono i decimali (quindi divido l'importo per 100)
092100000000     C                   Z-Add     RBIM10        WWImporto
092200000000     C                   If        RBTCDD = 'E'
092300000000     C                   DIV       100           WWImporto
092400000000     C                   EndIf
092500000000     C                   ADD       WWImporto     WWIMSP
092600000000     C                   ENDIF
092700000000      *
092800000000     C*****              EXCEPT    PRT010
092900000000      *
093000000000     C                   ENDSR
093100000000     C*------------------------------------------------------------------*
093200000000     C* Fine carico: scrivo record di testata su NDRTK00F
093300000000     C*------------------------------------------------------------------*
093400000000     C     ENDCAR        BEGSR
093500000000      *
093600000000     C     RTKSBIS       IFEQ      *ZERO
093700000000     C     WWIMSP        ANDGT     *ZERO
093800000000     C                   Z-ADD     WWIMSP        RTKSBIS
093900000000     C                   Z-ADD     0             WWIMSP
094000000000     C                   ENDIF
094100000000      *
094200090128D2474x*                  Eval      $Struttura = 'ANBRA'
094300090128  "  x*                  Eval      $LenOut = %Size(ANBRADS)
094400090128  "  x*                  Callb     'NDDVABRA'
094500090128  "  x*                  PARM      '01'          $Operazion
094600090128  "  x*                  PARM                    XScSoc
094700090128  "  x*                  PARM                    EfaBanca
094800090128  "  x*                  PARM                    LineaK37
094900090128  "  x*                  PARM                    *Omit
095000090128  "  x*                  PARM                    *Omit
095100090128  "  x*                  PARM                    $Struttura
095200090128  "  x*                  PARM                    $LenOut
095300090128  "  x*                  PARM                    $Esito
095400090128  "  x*                  PARM                    ANBRADS
095500090128  "  x*                  If        $Esito = *Off
095600090128  "  x*                  Move      BraKcc        RTKBKCC
095700090128  "  x*                  Move      BraKsc        RTKBKSC
095800090128D2474x*                  EndIf
095900000000      *
096000000000     C                   Move      KccEK37       RTKEKCC
096100000000     C                   Move      KscEK37       RTKEKSC
096200000000     C                   If        RTKEKSC = *Blank
096300000000     C                   Move      RTKBKSC       RTKEKSC
096400000000     C                   EndIf
096500000000      *
096600000000      * Non scrivo testata se non ci sono insoluti nella contabile
096700000000     C                   If        $InsoCont = *On
096800000000     C                   WRITE     NDRTK000
096900041005C1790c                   z-add     0             Rtksbis
097000041005C1790c                   z-add     0             Rtksbii
097100000000     C                   EndIf
097200000000      *
097300000000     C                   ENDSR
097400090128D2474C*------------------------------------------------------------------*
097500090128     C* Reperimento  banca di presentazione per ogni effetto
097600090128D2474C*------------------------------------------------------------------*
097700090128     C     RepBanca      BEGSR
097800090128      *
097900090128      *
098000090128     C                   Eval      $Struttura = 'ANBRA'
098100090128     C                   Eval      $LenOut = %Size(ANBRADS)
098200090128     C                   Callb     'NDDVABRA'
098300090128     C                   PARM      '01'          $Operazion
098400090128     C                   PARM                    XScSoc
098500090128     C                   PARM                    EfaBanca
098600090128     C                   PARM                    LineaK37
098700090128     C                   PARM                    *Omit
098800090128     C                   PARM                    *Omit
098900090128     C                   PARM                    $Struttura
099000090128     C                   PARM                    $LenOut
099100090128     C                   PARM                    $Esito
099200090128     C                   PARM                    ANBRADS
099300090128     C                   If        $Esito = *Off
099400090128     C                   Move      BraKcc        RTKBKCC
099500090128     C                   Move      BraKsc        RTKBKSC
099600090128     C                   EndIf
099700090128     c*
099800090128D2474c                   ENDSR
099900000000     C*------------------------------------------------------------------*
100000000000     C* Ultime opearzioni per LR / Fine del programma
100100000000     C*------------------------------------------------------------------*
100200000000     C     ENDPGM        BEGSR
100300000000      *
100400000000      * Stampa riepilogo
100500000000     C                   EXSR      SRPRT
100600000000      *
100700000000      * Fissa i parametri (fine eleborazione)
100800000000     C     FlgCarK37     IFEQ      'E'
100900000000     C                   MOVE      *Blank        FlgCarK37
101000000000     C                   movel     NdcK37ds      XPaOut
101100000000     C                   CALLB     'XPAR'
101200000000     C                   parm                    XScSoc
101300000000     C                   parm      'HBINRIBA'    XPaPar
101400000000     C                   parm                    XPaOut
101500000000     C                   parm      *OFF          XPaErr
101600000000     C                   parm      '1'           XPaRic            1
101700000000     C                   ENDIF
101800000000      *
101900000000     C                   ENDSR
102000000000     C*------------------------------------------------------------------*
102100000000     C* DEFINIZIONE CAMPI E CHIAVI E OPERAZIONI INIZIALI
102200000000     C*------------------------------------------------------------------*
102300000000     C     *INZSR        BEGSR
102400000000     C*
102500000000     C     *ENTRY        PLIST
102600000000     C                   PARM                    KPJBA
102700000000     C*
102800000000     C* Reperimento dati società
102900000000     C*
103000000000    >C                   MOVEL     'SOC001'      TIPXSC
103100000000     C                   MOVEL     *BLANK        SOCXSC
103200000000     C                   EXSR      REPSOC
103300000000     C     RTNXSC        IFNE      '1'
103400000000     C                   MOVEL     XSOCDS        SOC001
103500000000     C                   ELSE
103600000000     C                   SETON                                        LR
103700000000     C                   GOTO      FINE
103800000000     C                   ENDIF
103900000000     C*
104000000000     C* Valorizzazione campi univoci testate
104100000000     C                   MOVEL     KNSIF         NOMSIF           10
104200000000     C                   MOVEL     XSCDSI        NOMDIT           20
104300000000     C                   MOVEL     DSPGM         NOMPGM           10
104400000000     C* Reperisco nome sistema informatico
104500000000     C                   CALL      'XNETA'                              21
104600000000     C                   PARM                    NOMSYS            8
104700000000     C*
104800000000     C                   TIME                    ORA               6 0
104900000000     C*
105000000000     C* GESTIONE RIEPILOGO
105100000000     C                   MOVE      *ZERO         RC                1 0
105200000000     C                   MOVE      *ZERO         D0                1 0
105300000000     C*
105400000000     C* Reperimento parametri
105500000000     C                   CALLB     'XPAR'
105600000000     C                   parm                    xscsoc
105700000000     C                   parm      'HBINRIBA'    xpapar            8
105800000000     C                   parm                    xpaout
105900000000     C                   parm      *off          xpaerr            1
106000000000     C                   parm      *blank        xparic            1
106100000000     C     xpaerr        ifeq      *on
106200000000     C                   SETON                                        LR
106300000000     C                   GOTO      FINE
106400000000     C                   else
106500000000     C                   movel     xpaout        ndck37ds
106600000000     C                   endif
106700000000     C*
106800000000     C* Fissa i parametri (elaborazione in corso)
106900000000     C                   MOVE      'E'           FlgCarK37
107000000000     C                   movel     NdcK37ds      XPaOut
107100000000     C                   CALLB     'XPAR'
107200000000     C                   parm                    XScSoc
107300000000     C                   parm      'HBINRIBA'    XPaPar
107400000000     C                   parm                    XPaOut
107500000000     C                   parm      *OFF          XPaErr
107600000000     C                   parm      '1'           XPaRic            1
107700000000     C*
107800000000     C* KLIST
107900000000     C*
108000000000     C     K04RDK01      KLIST
108100000000     C                   KFLD                    XScSoc
108200000000     C                   KFLD                    RDKDS14
108300000000     C                   KFLD                    RDKDSOR
108400000000     C                   KFLD                    RDKDSCA
108500000000     C*
108600000000     C     K03EFA02      KLIST
108700000000     C                   KFLD                    XScSoc
108800000000     C                   KFLD                    EFACTB
108900000000     C                   KFLD                    EFANREFF
109000000000     C*
109100041005C1790C     K04EFA03      KLIST
109200000000"    C                   KFLD                    XScSoc
109300000000"    C                   KFLD                    EFACTB
109400000000"    C                   KFLD                    EFAKSC
109500041005C1790C                   KFLD                    RBDSCA10
109600000000     C*
109700000000     C     K02REG01      KLIST
109800000000     C                   KFLD                    RegSys
109900000000     C                   KFLD                    RegNrAsReg
110000000000     C*
110100000000     C* PLIST
110200000000     C*
110300000000     C     PNMR          PLIST
110400000000     C                   PARM                    XNMRIC            1
110500000000     C                   PARM                    XNMSOC            3
110600000000     C                   PARM                    XNMUNI            8
110700000000     C                   PARM                    XNMCTB            2
110800000000     C                   PARM                    XNMKEY            8
110900000000     C                   PARM                    XNMSER            4
111000000000     C                   PARM                    XNMCMT            1
111100000000     C                   PARM                    XNMDAT           10
111200000000     C                   PARM                    XNMIVA            1
111300000000     C                   PARM                    XNMREG            1
111400000000     C                   PARM                    XNMERR            1
111500000000     C                   PARM                    XNMNUM            9 0
111600000000     C                   PARM                    XNMAUT            1
111700000000     C*
111800000000     C     *DMY          Move      UDate         Oggi
111900000000C1309C     *JobRun       Move      Oggi          ZDate             6 0
112000000000     C*
112100000000     C                   Move      *Off          $PrimoPrg         1
112200000000     C*
112300000000     C                   MOVE      XScSoc        RDKSocieta
112400000000     C                   MOVE      XScSoc        RTKSocieta
112500000000     C*
112600000000     C                   ENDSR
112700000000     C******************************************************
112800000000     C* REPERIMENTO NR. PROGRESSIVO DI CARICO
112900000000     C******************************************************
113000000000     C     REPPRGCAR     BEGSR
113100000000     C*
113200000000     C                   MOVE      '3'           XNMRIC
113300000000     C                   MOVE      XScSoc        XNMSOC
113400000000     C                   MOVE      *Blank        XNMUNI
113500000000     C                   MOVE      *Blank        XNMCTB
113600000000     C                   MOVE      *Blank        XNMKEY
113700000000     C                   MOVE      'HOBA'        XNMSER
113800000000     C                   MOVE      *Off          XNMCMT
113900000000     C                   MOVE      *Off          XNMIVA
114000000000     C                   MOVE      *Off          XNMREG
114100000000     C                   MOVE      Oggi          XNMDAT
114200000000     C*
114300000000     C                   CALLB     'XNMR'        PNMR
114400000000     C     XNMERR        IFNE      '0'
114500000000     C                   SETON                                        LR
114600000000     C                   GOTO      FINE
114700000000     C                   ELSE
114800000000     C                   Z-ADD     XNMNUM        PrgCarico         9 0
114900000000     C                   ENDIF
115000000000     C*
115100000000     C                   If        $PrimoPrg = *Off
115200000000     C                   Z-Add     PrgCarico     PrgCaricoI        9 0
115300000000     C                   Move      *On           $PrimoPrg
115400000000     C                   EndIf
115500000000     C*
115600000000     C                   ENDSR
115700000000      ************************************************************
115800000000      * Reperimento dati società
115900000000      ************************************************************
116000000000     C     REPSOC        BEGSR
116100000000      *
116200000000     C                   CALLB     'XSOC'
116300000000     C                   PARM                    TIPXSC            6
116400000000     C                   PARM                    SOCXSC            3
116500000000     C                   PARM                    CDSXSC            9 0
116600000000     C                   PARM                    MODXSC            3
116700000000     C                   PARM      *BLANK        RTNXSC            1
116800000000     C                   PARM                    XSOCDS
116900000000     C                   PARM                    KPJBA
117000000000      *
117100000000     C                   ENDSR
117200000000      *----------------------------------------------------------------
117300000000     OQSYSPRT   H    1P                       01
117400000000     O         OR    OF
117500000000     O                       NOMDIT              21
117600000000     O                       KNMUS              110
117700000000C1309O*****                  UDATE         Y    120
117800000000C1309O                       ZDate         Y    120
117900000000     O                                          126 'Pag.'
118000000000     O                       PAGE          Z    131
118100000000     O          H    1P                       02
118200000000     O         OR    OF
118300000000     O                       NOMSYS               9
118400000000     O                       NOMSIF              20
118500000000     O                                           61 '*** CARICAMENTO ESITI RI.B'
118600000000     O                                           80 'A. HOME BANKING ***'
118700000000     O                       NOMPGM             110
118800000000     O                       ORA                120 '  :  :  '
118900000000     O          H    1P                     2 03
119000000000     O         OR    OF
119100000000     O                                          110 'gruppo PRO'
119200000000     O          H    1P                     2
119300000000     O         OR    OFN90
119400000000     O                                              'RECORD'
119500000000D0650O                                         +  2 'NR.EFF.'
119600000000D0650X*****                                    +  2 ' NR.EFF.'
119700000000     O                                         +  2 'CLIENTE '
119800000000     O                                         +  2 '  IMPORTO RICEVUTA   '
119900000000     O                                         +  1 'SCADENZA'
120000000000     O                                         +  2 'VALUTA  '
120100000000     O                                         +  1 'DATA RIF.'
120200000000     O                                         +  2 'NR.RIF.'
120300000000     O                                         +  2 'RAGIONE SOCIALE    '
120400000000     O                                         + 15 'BANCA'
120500000000     O                                         +  1 'E'
120600000000     O          EF           PRTERR         1
120700000000     O                     21                       'ERRORE'
120800000000     O                       RBDSOR        Z   +  2
120900000000     O                       FLG(1)
121000000000     O                       RBAKSC            +  1
121100000000     O                       FLG(2)
121200000000D0638X*****                  RBIMPO        K   +  1
121300000000D0638O                       RSIMPO        K
121400000000     O                       FLG(3)
121500000000     O                       RBDSCA            +  1 '  /  /  '
121600000000     O                       RBVSIA            +  1 '  /  /  '
121700000000D0638X*****                  RSDRIF         B  +  1
121800000000  "  X*****                  RSNRIF         B  +  1
121900000000  "  O                       RSDRIF         B  +  3
122000000000  "  O                       RSNRIF         B  +  4
122100000000  "  X*****                  RBDRS1         B  +  1
122200000000D0638O                       RBDRS1         B  +  3
122300000000     O                       PRTPRV            +  1
122400000000     O                       RBDABI            +  1
122500000000     O                       FLG(4)            +  1
122600000000     O          E   N22 21   PRTERR         1
122700000000     O                                              'NDEFA '
122800000000D0638O               29                          21 'NON TROVATO !!!'
122900000000  "  O              N29      EFANREFF      Z     14
123000000000  "  O              N29      EFAKSC            +  3
123100000000  "  O              N29      EFWIMD        K   +  2
123200000000  "  O              N29      EFWDSC            +  3 '  /  /  '
123300000000  "  O              N29      EFADIVISA         +  3
123400000000  "  O              N29      EFWDRP            +  4 '  /  /  '
123500000000  "  O              N29      EFANRPAR      Z
123600000000  "  O              N29      EFAIMPORTO    K   +  3
123700000000  "  X*****exD0650   29                        +  1 'NON TROVATO !!!'
123800000000  "  X*****exD0650  N29      EFAKSC            + 13
123900000000  "  X*****exD0650  N29      EFAIMPORTO    K   +  7
124000000000  "  X*****exD0650  N29      EFWDSC            +  4 '  /  /  '
124100000000  "  X*****exD0650  N29      EFANREFF      Z   +  6
124200000000  "  X*****exD0650  N29      EFWDRP            +  1 '  /  /  '
124300000000D0638X*****exD0650  N29      EFANRPAR      X   +  2
124400000000D0650X*****                  EFAKSC            + 13
124500000000  "  X*****                  EFAIMPORTO    K   +  7
124600000000  "  X*****                  EFWDSC            +  4 '  /  /  '
124700000000  "  X*****                  EFANREFF      Z   +  6
124800000000  "  X*****                  EFWDRP            +  1 '  /  /  '
124900000000D0650X*****                  EFANRPAR      X   +  2
125000000000     O          EF           PRT010      1  1
125100000000     O                       RBCA10               5
125200000000     O                       RBOC10              25
125300000000     O                       RBIM10        K     47
125400000000     O                       RBVL10              71 '  /  /  '
125500000000     O          EF           PRT0EF         1
125600000000     O                                              ' '
125700000000     O          E            PRTERR         1
125800000000     O          EF           BADSEQ         1
125900000000     O                       COL
126000000000     O          E            BADSEQ         2
126100000000     O                       RECORD
126200000000     O          E            HDRTOT         0 66
126300000000     O          EF           HDRTOT      2  3
126400000000     O                                              'RIEPILOGO CARICAMENTO '
126500000000     O                                              'DA NR.'
126600000000     O                       PrgCaricoI    Z
126700000000     O                                              ' A NR.'
126800000000     O                       PrgCarico     Z
126900000000     O                                         +  6 'DATA '
127000000000C1309O*****                  UDATE         Y
127100000000C1309O                       ZDate         Y
127200000000     O                                         +  6 'ORA '
127300000000     O                       RTKCART                '  :  :  '
127400000000     O          E            HDRTOT         2
127500000000     O                                              'ESITO'
127600000000     O                                           52 'CORRETTI '
127700000000     O                                           88 'ERRATI '
127800000000     O                                          124 'TOTALE '
127900000000     O          E            TOTAL          1
128000000000     O                       CST(RC)
128100000000     O                                         +  2 'NUMERI  '
128200000000     O                       DNU(1)        K     52
128300000000     O                       DNU(2)        K     88
128400000000     O                       DNU(3)        K    124
128500000000     O          E            TOTAL          2
128600000000     O                       CSTIMP            + 12
128700000000     O                       DIM           K   +  2
128800050112D1563O          E            PRTERR1        1
128900050112 "   O                                              'ERRORE'
129000050112 "   O                       RBDSOR        Z   +  2
129100050112 "   O                                         +  1 'Causale errata tab. I07'
129200050112D1563O                       RBCAUS            +  1
129300000000**  Costante x XMSGR
129400000000PROMSG    *LIBL     COS0182    avvertimento elaborazione
129500000000** COSTANTI (CST)
129600000000PAGATI
129700000000STORNATI
129800000000INSOLUTI
129900000000ALTRI
130000000000*BADSEQ
130100000000    TOTALI
130200000000** COLONNE (COL)
130300000000*... ... 1 ... ... 2 ... ... 3 ... ... 4 ... ... 5 ... ... 6
130400000000 ... ... 7 ... ... 8 ... ... 9 ... ... 0 ... ... 1 ... ... 2
