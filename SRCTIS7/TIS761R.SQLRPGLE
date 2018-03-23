000100021126      *PARMS DYNUSRPRF(*OWNER) TGTRLS(V4R5M0)
000200000707     H dftactgrp(*yes)
000300970526     H*------------------------------------------------------------------------*
000400991112     H* Esecuzione OPZIONI Gestione log TIVLR00F - Strategi Upload
000500970526     H*------------------------------------------------------------------------*
000600970526     H DECEDIT('0,') DATEDIT(*DMY.)
000700970526     F*------------------------------------------------------------------------*
000800970526     F* DATA BASE
000900970526     F*------------------------------------------------------------------------*
001000970526     F*------------
001100991112     Ftivlr03l  UF A E           K DISK
001200021125     Ftivin00r  UF A E           K DISK
001300080411     Ftivin00s  UF   E           K DISK    prefix (S_)
001400021126     Ftivlr06l  UF   E           K DISK    rename(tivlr000:tivlr006)
001500021126     F                                     usropn prefix(U_)
001600970526     D*------------------------------------------------------------------------*
001700970526     D* SCHIERE
001800970526     D*------------------------------------------------------------------------*
001900991112     D*------------
002000991112     D* ARCHITETTURA
002100991112     D*------------
002200991112     D KPJBA         E DS
002300991110     D*------------
002400991112     D* PASSAGGIO PARAMETRI
002500991110     D*------------
002600991112     D PARAM           DS                  INZ
002700991112     D  PLIB                   1     10
002800991112     D  POBJ                  11     20
002900991112     D  PMBR                  21     30
003000991112     D  PDAT                  31     38  0
003100991112     D  PORA                  39     44  0
003200991112     D  PKSC                  45     52
003300991112     D  PFLG                  53     53
003400991112     D  PTIP                  54     55
003500991112     D  PPOI                  56     58  0
003600991112     D  PSQZ                  59     61  0
003700991112     D  POPZ                  62     63
003800021125     D  PFL3                  64     73
003900030616     D  PMSG                  74    103
004000090519     D  PMBRNEW              104    113
004100090519
004200000707
004300010314     D*------------
004400010314     D* VARIABILI DI WRK
004500010314     D*------------
004600000707     D  maximum        s                   like(vlrsqz) inz(*zeros)
004700000707
004800010524     D*------------
004900010524     D* DS RIDEFINIZIONE FLAG OPERATIVI FILE LOG UPLOAD (TIVLR00F)
005000010524     D*------------
005100010524     D DSVLR3        E DS
005200010524
005300010524
005400970526     C*------------------------------------------------------------------------*
005500970526     C* MAIN LINES
005600970526     C*------------------------------------------------------------------------*
005700970526     C*
005800991112     C                   Z-ADD     PDAT          KLRdat
005900991112     C                   Z-ADD     PORA          KLRora
006000991112     C                   MOVEL     PKSC          KLRksc
006100991112     C                   MOVEL     PFLG          KLRflg
006200991112     C                   MOVEL     PTIP          KLRtip
006300991112     C                   Z-ADD     PPOI          KLRpoi
006400991112     C                   Z-ADD     PSQZ          KLRsqz
006500991112     C*
006600991112     C* VERIFICA DI SICUREZZA LETTURA FILE DI LOG
006700991112     C*
006800991112     C     KEYvlr        CHAIN     tivlr03l                           99
006900991112     C     *IN99         IFEQ      *OFF
007000090519     C*
007100090519     C                   SETOFF                                       55
007200991112     C*
007300991112     C     POPZ          IFEQ      ' 1'
007400090519     C                   SETON                                        55
007500090519     C     VLRflg        IFEQ      '1'
007600090519     C     VLRflg        OREQ      '3'
007700090519     C                   MOVEL     '3'           VLRflg
007800090519     C                   UPDATE    tivlr000
007900090519     C                   ENDIF
008000991112     C     VLRflg        IFEQ      '2'
008100991112     C     VLRflg        OREQ      '4'
008200991112     C                   MOVEL     '4'           VLRflg
008300991112     C                   UPDATE    tivlr000
008400991112     C                   ENDIF
008500090519     C                   EXSR      DUPSTO
008600090519     C                   EXSR      NEWLOG
008700991112     C                   ENDIF
008800991112     C*
008900991112     C     POPZ          IFEQ      ' 4'
009000991112     C                   DELETE    tivlr000
009100991112     C                   ENDIF
009200991112     C*
009300991112     C     POPZ          IFEQ      '11'
009400991112     C     VLRflg        IFEQ      '1'
009500991112     C     VLRflg        OREQ      '3'
009600991112     C                   MOVEL     '3'           VLRflg
009700991112     C                   UPDATE    tivlr000
009800991112     C                   ENDIF
009900991112     C     VLRflg        IFEQ      '2'
010000991112     C     VLRflg        OREQ      '4'
010100991112     C                   MOVEL     '4'           VLRflg
010200991112     C                   UPDATE    tivlr000
010300991112     C                   ENDIF
010400021126     C                   EXSR      MODSTO
010500991112     C                   EXSR      NEWLOG
010600991112     C                   ENDIF
010700991112     C*
010800991112     C     POPZ          IFEQ      '14'
010900021119     C***                ADD       1             VLRsqz
011000021119     C***                MOVEL     *BLANKS       VLRlis
011100021119     C***                MOVEL     *BLANKS       VLRfis
011200021119     C***                UPDATE    tivlr000
011300021119     C                   DELETE    tivlr000
011400991112     C                   ENDIF
011500991112     C*
011600991112     C                   ENDIF
011700991112     C*
011800970526     C* FINE PGM
011900970526     C                   SETON                                        LR
012000991112     C*------------------------------------------------------------------------*
012100991112     C* NEWLOG - MODIFICA CAMPI RECORD E SUA SCRITTURA
012200991112     C*------------------------------------------------------------------------*
012300991112     C     NEWLOG        BEGSR
012400000707     C*
012500000707     C*
012600000707     C/EXEC SQL
012700000707     C+ declare C1 cursor for select max(vlrsqz) from tivlr00f where
012800000707     C+ vlrdat = :PDAT and vlrora = :PORA and vlrksc = :PKSC and
012900000707     C+ vlrtip = :PTIP
013000000707     C+ for fetch only
013100000707     C/END-EXEC
013200000707     C
013300000707     C/EXEC SQL
013400000707     C+ open C1
013500000707     C/END-EXEC
013600000707     C
013700000707     C/EXEC SQL
013800000707     C+ Fetch C1 into :maximum
013900000707     C/END-EXEC
014000000707     C*
014100000707     C*
014200000707     C                   IF        sqlcod = *zeros
014300000707     C                   ADD       1             maximum
014400000707     C                   Z-ADD     maximum       VLRsqz
014500991112     C                   MOVEL     '0'           VLRflg
014600030613     C                   MOVE(P)   PMSG          VLRmsg
014700010524     C                   MOVEL(P)  VLRFL3        DSVLR3
014800010524     C                   MOVEL     '1'           §VLR31
014900021126     C                   MOVEL     *blanks       §VLR33
015000010524     C                   MOVEL     DSVLR3        VLRFL3
015100011127     C                   MOVEL     *blanks       VLRPGT
015200011127     C                   MOVEL     *blanks       VLRPPT
015300090519     C   55              MOVEL     PMBRNEW       VLRPRG
015400090526     C   55              MOVEL     PMBRNEW       VLRHDL
015500991112     C                   WRITE     tivlr000
015600000707     C                   ENDIF
015700000707     C
015800000707     C/EXEC SQL
015900000707     C+ close C1
016000000707     C/END-EXEC
016100000707     C
016200991112     C*
016300991112     C                   ENDSR
016400991112     C*------------------------------------------------------------------------*
016500991112     C* MODSTO - MODIFICA RECORDS FILE/MEMBRO FILE STORICO
016600991112     C*------------------------------------------------------------------------*
016700991112     C     MODSTO        BEGSR
016800991112     C*
016900021125     C* ...se il flag di storicizzazione nn è valorizzato => TIVIN00R
017000021125     C                   IF        PFL3 <> 'S'
017100991112     C                   READ      tivin00r                               89
017200991112     C     *IN89         DOWEQ     *OFF
017300040730     C                   MOVEL     *BLANKS       VINflg
017400991112     C                   MOVEL     *BLANKS       VINmsg
017500991112     C                   UPDATE    tivin000
017600991112     C                   READ      tivin00r                               89
017700991112     C                   ENDDO
017800021125     C                   ELSE
017900031014     C* ...se il flag di storicizzazione è valorizzato => TIVIN00S
018000030424     C     PMBR          SETLL     TIVIN00S
018100030424     C                   IF        %equal(TIVIN00S)
018200030424     C     PMBR          READE     TIVIN00S
018300021125     C                   DOW       not %eof(TIVIN00S)
018400080411     C                   EVAL      vinDTA = %trimr(S_vinDTA)
018500040730     C                   MOVEL     *BLANKS       VINflg
018600021125     C                   MOVEL     *BLANKS       VINmsg
018700021125     C                   WRITE(e)  TIVIN000
018800021125     C                   IF        not %error
018900021125     C                   DELETE    TIVINSSS
019000021125     C                   ENDIF
019100021125     C     PMBR          READE     TIVIN00S
019200021125     C                   ENDDO
019300021126     C* A questo punto riaggiorno tutti i record d log correlati al membro/chiave corrente
019400021126     C* inizializzando il flag che indica l'avvenuta storicizzazione.
019500021126     C                   EXSR      UPDLOG
019600021125     C                   ENDIF
019700021125     C                   ENDIF
019800991112     C*
019900991112     C                   ENDSR
020000090519     C*------------------------------------------------------------------------*
020100090519     C* DUPSTO - DUPLICA RECORDS FILE/MEMBRO FILE STORICO
020200090519     C*------------------------------------------------------------------------*
020300090519     C     DUPSTO        BEGSR
020400090519     C*
020500090519     C* ...se il flag di storicizzazione nn è valorizzato => TIVIN00R
020600090519     C                   IF        PFL3 <> 'S'
020700090519     C                   READ      tivin00r                               89
020800090519     C     *IN89         DOWEQ     *OFF
020900090519     C                   MOVEL     *BLANKS       VINflg
021000090519     C                   MOVEL     *BLANKS       VINmsg
021100090519     C                   UPDATE    tivin000
021200090519     C                   READ      tivin00r                               89
021300090519     C                   ENDDO
021400090519     C                   ELSE
021500090519     C* ...se il flag di storicizzazione è valorizzato => TIVIN00S
021600090519     C     PMBR          SETLL     TIVIN00S
021700090519     C                   IF        %equal(TIVIN00S)
021800090519     C     PMBR          READE     TIVIN00S
021900090519     C                   DOW       not %eof(TIVIN00S)
022000090519     C                   EVAL      vinDTA = %trimr(S_vinDTA)
022100090519     C                   MOVEL     *BLANKS       VINflg
022200090519     C                   MOVEL     *BLANKS       VINmsg
022300090519     C                   WRITE(e)  TIVIN000
022400090519     C     PMBR          READE     TIVIN00S
022500090519     C                   ENDDO
022600090519     C                   ENDIF
022700090519     C                   ENDIF
022800090519     C*
022900090519     C                   ENDSR
023000021126     C*------------------------------------------------------------------------*
023100021126     C* UDPLOG - AGGIORNAMENTO RECORD DI LOG LEGATI AL MEMBRO RICHIESTO
023200021126     C*------------------------------------------------------------------------*
023300021126     C     UPDLOG        BEGSR
023400021126     C*
023500021126     C* Aggiorno tutti i record di log legati al membro storicizzato azzerando l'avvenuta storicizz.
023600021126     C                   if        not %open(tivlr06l)
023700021126     C                   open      tivlr06l
023800021126     C                   endif
023900021126     C*
024000021126     C     K06vlr        setll     tivlr06l
024100021126     C                   if        %found(tivlr06l)
024200021126     C     K06vlr        reade     tivlr06l
024300021126     C                   dow       not %eof
024400021126     C                   if        %subst(U_vlrhdl:2:9) = %subst(PMBR:2:9)
024500021126     C                   movel(p)  U_vlrfl3      dsvlr3
024600021126     C                   eval      §VLR33 = *blanks
024700021126     C                   movel(p)  dsvlr3        U_vlrfl3
024800021126     C                   update    tivlr006
024900021126     C                   endif
025000021126     C     K06vlr        reade     tivlr06l
025100021126     C                   enddo
025200021126     C                   endif
025300021126     C*
025400021126     C                   if        %open(tivlr06l)
025500021126     C                   close     tivlr06l
025600021126     C                   endif
025700021126     C*
025800021126     C                   ENDSR
025900970526     C*------------------------------------------------------------------------*
026000970526     C* *INZSR - OPERAZIONI INIZIALI
026100970526     C*------------------------------------------------------------------------*
026200970526     C     *INZSR        BEGSR
026300970526     C*---
026400970526     C* RICEVIMENTO PARAMETRI
026500970526     C*---
026600970526     C     *ENTRY        PLIST
026700970612     C                   PARM                    KPJBA
026800991112     C                   MOVEL     KPJBU         PARAM
026900991108     C*---
027000991112     C     *LIKE         DEFINE    vlrdat        klrdat
027100991112     C     *LIKE         DEFINE    vlrora        klrora
027200991112     C     *LIKE         DEFINE    vlrksc        klrksc
027300991112     C     *LIKE         DEFINE    vlrflg        klrflg
027400991112     C     *LIKE         DEFINE    vlrtip        klrtip
027500991112     C     *LIKE         DEFINE    vlrpoi        klrpoi
027600991112     C     *LIKE         DEFINE    vlrsqz        klrsqz
027700970526     C*---
027800970526     C* CHIAVI DI LETTURA
027900970526     C*---
028000991112     C* LETTURA tivlr03L -
028100991108     C     KEYvlr        KLIST
028200991112     C                   KFLD                    KLRdat                         *PARTENZA/ARRIVO
028300991112     C                   KFLD                    KLRora                         *PARTENZA/ARRIVO
028400991112     C                   KFLD                    KLRksc                         *PARTENZA/ARRIVO
028500991112     C                   KFLD                    KLRtip                         *PARTENZA/ARRIVO
028600991112     C                   KFLD                    KLRpoi                         *PARTENZA/ARRIVO
028700991112     C                   KFLD                    KLRsqz                         *PARTENZA/ARRIVO
028800991112     C                   KFLD                    KLRflg                         *PARTENZA/ARRIVO
028900021126     C* LETTURA tivlr06L -
029000021126     C     K06vlr        klist
029100021126     C                   kfld                    vlrprg
029200021126     C                   kfld                    vlrksc
029300021126     C                   kfld                    vlrtip
029400991112     C*
029500991112     C                   ENDSR
