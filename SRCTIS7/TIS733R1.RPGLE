000100970526     H*------------------------------------------------------------------------*
000200010917     H* ESTENSIONE ANAGRAFICA CLIENTI ABILITATI UTILITY RI-ASSEGNAZIONE PASSWORD
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800010917     Ftiabl01L  UF   E           K DISK
000900010918     Ftivlr04L  IF   E           K DISK
001000010918     Ftivlt04L  IF   E           K DISK
001100010918     Ftivtl02L  IF   E           K DISK
001200010917     FPRTF198   O    F  198        PRINTER
001300970526     D*------------------------------------------------------------------------*
001400970526     D* INPUT
001500970526     D*------------------------------------------------------------------------*
001600010917     D* CREAZOINE PASSWORD CLINTE
001700010917     D*------------
001800010917     D PASS            S              1    DIM(30)
001900010917     D*------------
002000970526     D* DS DI PROCEDURA
002100970526     D*------------
002200001020     D tis732        E DS                  EXTNAME(tis732ds)
002300010917     D*------------
002400010917     D* DS "XSRDA8" - CONTROLLA DATA (8)
002500010917     D*------------
002600010917     D WLBDA8          DS                  INZ
002700010917     D  G08DAT                 1      8  0
002800010917     D  G08INV                 9     16  0
002900010917     D  G08ERR                17     17
003000010917     D  G08TGI                18     22  0
003100010917     D*------------
003200010917     D* DS DI REPERIMENTO DATI CLIENTE
003300010917     D*------------
003400010917     DBS69DS         e ds                  extname(TIBS69DS)
003500010917     DACODS          e ds                  extname(cnaco00f)
003600010917     DINDDS          e ds                  extname(cnind00f)
003700010917     DCLPDS          e ds                  extname(cnclp00f)
003800010917     DCLSDS          e ds                  extname(fncls00f)
003900001020     D*------------
004000010313     D* VARIABILI DI WRK
004100010917     D*------------
004200010313     D API             s              1    inz('''')
004300010917     D POS             s              2  0 inz(*zeros)
004400010917     D ALFABETO        s             26    inz('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
004500010917     D rigaPRT         s            190    inz(*blanks)
004600010918     D contaMOD        s              5  0 inz(*zeros)
004700970526     C*------------------------------------------------------------------------*
004800970526     C* MAIN LINES
004900970526     C*------------------------------------------------------------------------*
005000970526     C*
005100010917     C* ESECUZIONE OPERAZIONI
005200010917     C                   EXSR      EXEOPE
005300970526     C* FINE PGM
005400970526     C                   SETON                                        LR
005500010917     C*------------------------------------------------------------------------*
005600010917     C* EXEOPE - ESEGUE LE OPERAZIONI
005700010917     C*------------------------------------------------------------------------*
005800010917     C     EXEOPE        BEGSR
005900010917     C*
006000010917     C     *loval        SETLL     tiabl01L
006100010917     C                   READ      tiabl01l
006200010917     C*
006300010917     C                   DOW       not %eof(tiabl01l)
006400010917     C                   IF        ablpwd = '@@@@@@@@@@@@@@@@@'
006500010917     C                   EXSR      MODIFIEDP
006600010917     C                   ENDIF
006700010917     C                   READ      tiabl01l
006800010917     C                   ENDDO
006900010918     C*
007000010918     C                   EXCEPT    listatot
007100010917     C*
007200010917     C                   ENDSR
007300010810     C*------------------------------------------------------------------------*
007400010810     C* MODIFIEDP - OPERAZIONI DI MODIFICA AD USO EDP
007500010810     C*------------------------------------------------------------------------*
007600010810     C     MODIFIEDP     BEGSR
007700010917     C*
007800010917     C                   EVAL      ERRL00 = '0'
007900010917     C*
008000010917     C* Verifico gli eventuali accessi dopo il 31/07/2001
008100010917     C                   EXSR      EXECHK
008200010917     C*
008300010917     C* Reperisce i dati del cliente
008400010917     C                   EXSR      DECTIB
008500010810     C*
008600010917     C* Calcola in automatico la password
008700010917     C                   EXSR      CRTPWD
008800010810     C*
008900010917     C* Se non ci sono errori eseguo i comandi remoti e l'aggiornamento del record
009000010810     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
009100010917     C*
009200010917     C* Effettuo controlli su Ragione Sociale e Responsabile ed effettuo serie comandi remoti
009300010917     C                   Z-ADD     *zeros        pos
009400010917     C                   EVAL      pos = %scan(api:wrkRAG)
009500010917     C                   DOW       pos > *zeros
009600010917     C                   EVAL      wrkRAG = %replace(' ':wrkRAG:pos)
009700010917     C                   Z-ADD     *zeros        pos
009800010917     C                   EVAL      pos = %scan(api:wrkRAG)
009900010917     C                   ENDDO
010000010917     C*
010100010917     C                   Z-ADD     *zeros        pos
010200010917     C                   EVAL      pos = %scan(api:ablRSP)
010300010917     C                   DOW       pos > *zeros
010400010917     C                   EVAL      ablRSP = %replace(' ':ablRSP:pos)
010500010917     C                   Z-ADD     *zeros        pos
010600010917     C                   EVAL      pos = %scan(api:ablRSP)
010700010917     C                   ENDDO
010800010810     C*
010900010917     C                   EVAL      rigaPRT = %trim(ablSUN) + ' ' +
011000010917     C                                       %trim(ablUTE) + ' ' +
011100010917     C                                       %trim(ablPWD)
011200010917     C                   EXCEPT    listamod
011300010918     C                   EVAL      contaMOD = contaMOD + 1
011400010917     C*
011500010919     C                   CALL(e)   'TIS730R1'
011600100906     C                   PARM      ablUTE        alfaute          40
011700010919     C                   PARM      ablPWD        alfapwd          20
011800010919     C                   PARM      wrkRAG        alfarag          20
011900010919     C                   PARM      ablRSP        alfarsp          30
012000010919     C                   PARM      ablSUN        alfasun           9
012100010919     C                   PARM      *blanks       esito             1
012200010919     C                   PARM      'A'           operazione        1
012300010919     C                   PARM      ablEML        alfaeml          78
012400010919     C                   IF        %error or esito = '1'
012500010919     C                   MOVEL     '1'           ERRL00
012600010919     C                   ELSE
012700010919     C                   IF        esito = '2'
012800010919     C                   MOVEL     '1'           ERRL00
012900010919     C                   ENDIF
013000010919     C                   ENDIF
013100010810     C*
013200010917     C* Se ancora no errori aggiorno il record
013300010810     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
013400010917     C                   EVAL      ablINV = '0'
013500010919     C                   UPDATE    tiabl000
013600010810     C                   ENDIF
013700010810     C                   ENDIF
013800010810     C*
013900010810     C                   ENDSR
014000010917     C*------------------------------------------------------------------------*
014100010917     C* EXECHK - Routine di verifica accessi dei clienti
014200010917     C*------------------------------------------------------------------------*
014300010917     C     EXECHK        BEGSR
014400010917     C*
014500010918     C                   EVAL      kxxKSC = '0'+ablUTE
014600010918     C* TIVLR04L
014700010917     C                   IF        ERRL00 = '0'
014800010918     C     kxxKSC        SETLL     tivlr04l
014900010918     C                   IF        %equal(tivlr04l)
015000010918     C     kxxKSC        READE     tivlr04l
015100010918     C                   DOW       not %eof(tivlr04l)
015200010919     C                   IF        vlrDAT >= 20010731
015300010917     C                   EVAL      ERRL00 = '1'
015400010918     C                   LEAVE
015500010917     C                   ENDIF
015600010918     C     kxxKSC        READE     tivlr04l
015700010918     C                   ENDDO
015800010917     C                   ENDIF
015900010918     C                   ENDIF
016000010918     C* TIVLT04L
016100010918     C                   IF        ERRL00 = '0'
016200010918     C     kxxKSC        SETLL     tivlt04l
016300010918     C                   IF        %equal(tivlt04l)
016400010918     C     kxxKSC        READE     tivlt04l
016500010918     C                   DOW       not %eof(tivlt04l)
016600010919     C                   IF        vltDAT >= 20010731
016700010918     C                   EVAL      ERRL00 = '1'
016800010918     C                   LEAVE
016900010918     C                   ENDIF
017000010918     C     kxxKSC        READE     tivlt04l
017100010918     C                   ENDDO
017200010918     C                   ENDIF
017300010918     C                   ENDIF
017400010918     C* TIVTL02L
017500010918     C                   IF        ERRL00 = '0'
017600010918     C     kxxKSC        SETLL     tivtl02l
017700010918     C                   IF        %equal(tivtl02l)
017800010918     C     kxxKSC        READE     tivtl02l
017900010918     C                   DOW       not %eof(tivtl02l)
018000010919     C                   IF        vtlDAT >= 20010731
018100010918     C                   EVAL      ERRL00 = '1'
018200010918     C                   LEAVE
018300010918     C                   ENDIF
018400010918     C     kxxKSC        READE     tivtl02l
018500010918     C                   ENDDO
018600010918     C                   ENDIF
018700010918     C                   ENDIF
018800010917     C*
018900010917     C                   ENDSR
019000010917     C*------------------------------------------------------------------------*
019100010917     C* CRTPWD - Creazione Password Cliente
019200010917     C*------------------------------------------------------------------------*
019300010917     C     CRTPWD        BEGSR
019400010917     C*
019500010917     C                   MOVEL     *blanks       ablPWD
019600010917     C                   MOVEA     wrkRAG        PASS
019700010917     C                   EXSR      CALORA
019800010917     C                   MOVE(p)   ORA           Alfa4             4
019900010917     C                   MOVEL     *blanks       Alfa1             1
020000010917     C                   Z-ADD     *zeros        Num1              1 0
020100010917     C     1             DO        30            x                 2 0
020200010917     C                   EVAL      Alfa1  = PASS(x)
020300010917     C                   Z-ADD     *zeros        pos
020400010917     C                   EVAL      pos = %scan(PASS(x):alfabeto)
020500010917     C                   IF        pos > 0 AND Num1 < 3
020600010917     C                   EVAL      ablPWD = %trim(ablPWD) + Alfa1
020700010917     C                   ADD       1             Num1
020800010917     C                   ENDIF
020900010917     C                   ENDDO
021000010917     C                   EVAL      ablPWD = %trim(ablPWD) + Alfa4
021100010917     C*
021200010917     C                   ENDSR
021300010917     C*------------------------------------------------------------------------*
021400010917     C* CALORA - Reperimento data ora di sistema in corso di esecuzione
021500010917     C*------------------------------------------------------------------------*
021600010917     C     CALORA        BEGSR
021700010917     C*
021800010917     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
021900010917     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
022000010917     C                   MOVEL     WN14          ORA               6            *ORA
022100010917     C                   Z-ADD     WN8           G08DAT
022200010917     C                   Z-ADD     *ZEROS        G08INV
022300010917     C                   MOVEL     '0'           G08ERR
022400010917     C                   CALL      'XSRDA8'
022500010917     C                   PARM                    WLBDA8
022600010917     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
022700010917     C                   MOVEL     DATCOR        AAAA              4            *ANNO ALFA
022800010917     C*
022900010917     C                   ENDSR
023000010917     C*------------------------------------------------------------------------*
023100010917     C* DECTIB - Reperimento RAgione Sociale del Cliente
023200010917     C*------------------------------------------------------------------------*
023300010917     C     DECTIB        BEGSR
023400010917     C*
023500010917     C                   CLEAR                   BS69DS
023600010917     C                   CLEAR                   ACODS
023700010917     C                   MOVE      ablUTE        I69KAC
023800010917     C                   CALL      'TIBS69R'
023900010917     C                   PARM                    BS69DS
024000010917     C                   PARM                    ACODS
024100010917     C                   PARM                    INDDS
024200010917     C                   PARM                    CLPDS
024300010917     C                   PARM                    CLSDS
024400010917     C     O69ERR        IFNE      '1'
024500010917     C                   MOVEL     ACORAG        wrkRAG           40
024600010917     C                   ELSE
024700010917     C                   MOVE      '1'           ERRL00
024800010917     C                   ENDIF
024900010917     C*
025000010917     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200001020     C* *INZSR - OPERAZIONI INIZIALI
025300970526     C*------------------------------------------------------------------------*
025400970526     C     *INZSR        BEGSR
025500010917     C*---
025600010917     C* VARIABILI RIFERITE AL DATABASE
025700010917     C*---
025800010918     C     *LIKE         DEFINE    vlrKSC        kxxKSC
025900970526     C*---
026000970526     C* VARIABILI DI CONTROLLO
026100970526     C*---
026200970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
026300970526     C*
026400970526     C                   ENDSR
026500010917
026600010918     OPRTF198   E            listamod    1
026700010917     O                       rigaPRT             +1
026800010918     O          E            listatot    1  1
026900010918     O                       contaMOD            +1
