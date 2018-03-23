000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200941025     H* FNLV18R  *---------------------------------------------------*
000300941024     H*     CALCOLO VOLUME AUTOMATICO                                *
000400921021     H*--------------------------------------------------------------*
000500941024     FTNTAM01L  IF   E           K DISK
000600941024     FTABEL00F  IF   E           K DISK
000700940218      *
000800941025     D* DS PER FNLV18R - DS DEI CAMPI PER CALCOLO VOLUME AUTOMATICO
000900941025     D DSLV18        E DS                  EXTNAME(FNLV18DS)
001000941109     D  D18DSP                18     25  0
001100020218
001200020311     d trul27ds      e ds
001300020218
001400941021     D*
001500020218     D**!!! DSQT1         E DS
001600921021     C*---------------------------------------------------------------*
001700921021     C     *ENTRY        PLIST
001800941025     C                   PARM                    DSLV18
001900000000     C*---------------------------------------------------------------*
002000941025     C* TIPO LANCIO, D18TLA "C" -           CHIUSO CON LR
002100941025     C* TIPO LANCIO, D18TLA "L" - ELABORO E CHIUDO CON LR
002200941025     C* TIPO LANCIO, D18TLA " " - ELABORO E CHIUDO IN RETRN
002300941025    1C     D18TLA        IFNE      'C'
002400941024     C*
002500941025     C* FLAG FRANCO/ASSEGNATO
002600941025     C                   MOVEL     D18TBL        FLGTB1            1
002700941025     C* CAMPO CON CUI VEDO SE E' TARIFFA CLIENTE O DI CARTELLO
002800941024     C                   MOVEL     D18KSC        W0050             5 0
002900941025     C* CAMPI DI COMODO
003000941025     C                   CLEAR                   WT8888
003100941025     C                   CLEAR                   WPRG
003200941025     C                   CLEAR                   W003A
003300941025     C*
003400941025     C* NON C'E' PROGRESSIVO, LO CERCO
003500941025    2C     D18PRG        IFEQ      *BLANKS
003600941024     C                   EXSR      TARCLI
003700941025     C                   ELSE
003800941024     C                   MOVEL     D18PRG        WPRG
003900941025    2C                   ENDIF
004000941025     C**
004100941025     C* CHAIN SU TARIFFA CLIENTE
004200941025     C**
004300941025    2C     WT8888        IFEQ      ' '
004400941024     C     KTAM          CHAIN     TNTAM000                           30
004500941025     C*
004600941024     C* SE NON C'E' RAPPORTO IN TARIFFA USO TARIFFA CARTELLO
004700941025    3C     *IN30         IFEQ      *ON
004800941024     C     TAMKPM        OREQ      *ZEROS
004900941025     C                   MOVEL     '1'           WT8888            1
005000941025    3C                   ENDIF
005100941025    2C                   ENDIF
005200941025     C**
005300941025     C* CHAIN SU TARIFFA DI CATELLO
005400941025     C**
005500941025    2C     WT8888        IFNE      ' '
005600941025     C*
005700941025     C* CERCO IL CODICE TARIFFA DELLA TARIFFA DI CARTELLO
005800021122    3C**!!!WRICER        IFEQ      ' '
005900941025     C                   EXSR      TARCAR
006000021122     C**!!!              MOVEL     '1'           WRICER            1
006100021122    3C**!!!              ENDIF
006200941025     C*
006300941024     C                   MOVEL     88888         W0050
006400941025     C*
006500941025     C     WSICAR        IFEQ      'S'
006600941025     C     KCAR2         CHAIN     TNTAM000                           30
006700941025     C                   ELSE
006800941025     C                   SETON                                        30
006900941025     C                   ENDIF
007000941025    2C                   ENDIF
007100941024     C*
007200941024     C* IMPOSTO FLAG: "P" SE PRESUNTO CON TARIFFA CARTELLO (FRANCO)
007300941024     C*                   SE E' ASSEGNATO  SEMPRE
007400941024     C*               "K" SE IN AUTOMATICO CON TARIFFA DEL CLI. A CHILO
007500941024     C*               "C" SE IN AUTOMATICO CON TARIFFA DEL CLI. A COLLO
007600941025    2C  N30W0050         IFEQ      88888
007700941024     C     FLGTB1        OREQ      'A'
007800941108     C                   MOVEL     'P'           D18FVF
007900941024     C                   ELSE
008000941108     C                   MOVEL     TAMTIN        D18FVF
008100941025    2C                   ENDIF
008200941025     C*
008300941025     C*  30 ON - NON TROVATE TARIFFE--> VOLUME "P"AT
008400941108     C   30              MOVEL     'P'           D18FVF
008500941025     C**
008600941024     C*      RAPPORTO KG PER METRO CUBO
008700941025     C**
008800950114    2C     TAMKPM        IFNE      0
008900941024     C* VEDO SE A KG O A COLLO
009000941025    3C     TAMTIN        IFEQ      'K'
009100941108     C     D18PKF        DIV(H)    TAMKPM        D18VLF
009200941025    3C                   END
009300941025    3C     TAMTIN        IFEQ      'C'
009400941024     C                   MOVE      TAMKPM        COMMPC            5 3
009500941108     C     COMMPC        MULT      D18NCL        D18VLF
009600941025    3C                   END
009700941025    2C                   END
009800941025     C*
009900941025     C* SE  VOLUME=0 IMPOSTO 0,001
010000941108     C     D18VLF        IFEQ      0
010100941108     C                   Z-ADD     0,001         D18VLF
010200941024     C                   END
010300941025     C                   END
010400941025     C*
010500941025     C* CHIUSURA PGM
010600941025    1C     D18TLA        IFEQ      ' '
010700941025     C                   RETURN
010800941025     C                   ELSE
010900941025     C                   SETON                                        LR
011000941025    1C                   ENDIF
011100941021     C*
011200941024     C* OPERAZIONI INIZIALI ------------------------------------------*
011300941024     C     *INZSR        BEGSR
011400941024     C     KTAM          KLIST
011500941024     C                   KFLD                    D18KSC
011600941024     C                   KFLD                    D18CTR
011700941024     C                   KFLD                    WPRG
011800941024     C     KTAM2         KLIST
011900941024     C                   KFLD                    D18KSC
012000941024     C                   KFLD                    D18CTR
012100941024     C* ACCESSO   TFTAM TARIFFA DI CARTELLO
012200941024     C     KCAR2         KLIST
012300941024     C                   KFLD                    TARKSC
012400941025     C                   KFLD                    TARCTR
012500941025     C                   KFLD                    TARPRG
012600941025     C     KTAB          KLIST
012700941025     C                   KFLD                    CODUT             1 0
012800941025     C                   KFLD                    COD               2
012900941025     C                   KFLD                    KEY               8
013000941024     C**
013100941024     C* DEFINIZIONE CAMPI
013200941024     C**
013300941024     C     *LIKE         DEFINE    TAMPRG        WPRG
013400941025     C     *LIKE         DEFINE    TAMPRG        TARPRG
013500941025     C     *LIKE         DEFINE    TAMCTR        TARCTR
013600941025     C     *LIKE         DEFINE    TAMKSC        TARKSC
013700941025     C***
013800941025     C* CARICO CAMPI FISSI PER TASSAZIONE/BOLLETTAZIONE
013900941025     C***
014000020218     C**!!!              Z-ADD     1             CODUT
014100020218     C**!!!              MOVEL     'QT'          COD
014200020218     C**!!!              MOVEL     '1       '    KEY
014300020218     C**!!!KTAB          CHAIN     TABEL                              30
014400020218     C**!!!*IN30         IFEQ      *OFF
014500020218     C**!!!TBLFLG        ANDEQ     ' '
014600020218     C**!!!              MOVEL     TBLUNI        DSQT1
014700020218     C**!!!              ELSE
014800020218     C**!!!              CLEAR                   DSQT1
014900020218     C**!!!              ENDIF
015000941025     C*
015100941024     C                   ENDSR
015200941024     C*
015300941024     C* CERCO LA TARIFFA DEL CLIENTE ---------------------------------*
015400941024     C     TARCLI        BEGSR
015500941025     C                   CLEAR                   W003A
015600071102     c* Per i codici 8888 o 9999 devo andared sulla cartello
015700071102     c                   move      d18ksc        w0040             4 0
015800071102     c
015900071102    0c                   if        w0040<>8888 and w0040<>9999
016000941025     C*
016100941024     C     KTAM2         SETLL     TNTAM000
016200941024     C     KTAM2         READE     TNTAM000                               30
016300941024     C*
016400941024    1C     *IN30         DOWEQ     *OFF
016500941024     C* SE ANNULLATA
016600941024    2C     TAMATB        IFEQ      'A'
016700941024     C* O BLOCCATA
016800941024     C     TAMBAP        OREQ      'B'
016900941024     C* O SOLO PER GLI ARRIVI ED E' BOLLA PARTENZA
017000941024     C     D18TBO        OREQ      'P'
017100941024     C     TAMBAP        ANDEQ     'A'
017200941024     C* O SOLO PER LE PARTENZE ED E' BOLLA ARRIVI
017300941024     C     D18TBO        OREQ      'A'
017400941024     C     TAMBAP        ANDEQ     'P'
017500941024     C* O ANCORA DA DECORRERE
017600941115     C     TAMDDT        ORGT      D18DSP
017700941024     C* RILEGGO
017800941024     C     KTAM2         READE     TNTAM000                               30
017900941025     C*
018000941025   X2C                   ELSE
018100941024     C*
018200941024     C* SE E' PROGRESSIVO VALIDO ESCO, ALTRIMENTI RILEGGO
018300941025     C                   MOVEL     TAMPRG        W003A             3
018400941024     C*
018500941115     C* SE TARIFFA SCADUTA LEGGO ALTRO PROGRESSIVO
018600941115     C* ALTRIMENTI ESCO DAL CICLO
018700941115    3C     TAMDST        IFLT      D18DSP
018800941024     C     KTAM2         READE     TNTAM000                               30
018900941115    3C                   ELSE
019000941115     C                   SETON                                        30
019100941025    3C                   ENDIF
019200941025    2C                   ENDIF
019300941025    1C                   ENDDO
019400071102    0C                   ENDif
019500941024     C*
019600941025     C* SE NON HO TROVATO NESSUN PROGRESSIVO PASSO SUBITO ALLA TARIFFA
019700941025     C* DI CARTELLO
019800941025     C     W003A         IFEQ      *BLANKS
019900941025     C                   MOVEL     '1'           WT8888
020000941025     C                   ELSE
020100941025     C                   CLEAR                   WT8888
020200941025     C                   MOVEL     W003A         WPRG
020300941025     C                   ENDIF
020400941025     C*
020500941024     C                   ENDSR
020600941024     C*
020700941024     C* CERCO LA TARIFFA DI CARTELLO ---------------------------------*
020800941024     C     TARCAR        BEGSR
020900941024     C*
021000941024     C* CARICO TARIFFA DI CARTELLO
021100020218     C**!!!              MOVEL     88888         TARKSC
021200020218     C**!!!              MOVE      §QTTCV        TARKSC
021300020218
021400020218      * Richiamo il trul27r per sapere qual'è la cartello
021500020218     c                   clear                   trul27ds
021600021121     c                   Movel     D18Ctr        I27Ctb
021700020218     c                   eval      i27tla = d18tla
021800020218     c                   eval      i27tsp = d18tsp
021900020218     c                   eval      i27lna = d18lna
022000020308     c                   eval      i27tln = d18tln
022100020218     c                   eval      i27lnp = d18lnp
022200020312     c                   eval      i27cli = d18ksc
022300020218     c                   eval      i27dta = d18dsp
022400020218     c                   call      'TRUL27R'
022500020218     c                   parm                    trul27ds
022600020627     c                   move      o27ksc        tarksc
022700020218     c                   movel     o27ctr        tarctr
022800020218     c                   movel     o27prg        tarprg
022900020218
023000980831     C                   CLEAR                   WSICAR
023100941024     C*
023200020218     C**!!!TARKSC        SETLL     TNTAM000
023300020218     C**!!!TARKSC        READE     TNTAM000                               30
023400941024     C*
023500020218    1C**!!!*IN30         DOWEQ     *OFF
023600941024     C*
023700941024     C* ESCLUDO SE ANNULLATO  O BLOCCATA
023800020218    2C**!!!TAMATB        IFEQ      'A'
023900941025     C* BLOCCATA
024000020218     C**!!!TAMBAP        OREQ      'B'
024100941025     C* O SOLO PER GLI ARRIVI ED E' BOLLA PARTENZA
024200020218     C**!!!D18TBO        OREQ      'P'
024300020218     C**!!!TAMBAP        ANDEQ     'A'
024400941025     C* O SOLO PER LE PARTENZE ED E' BOLLA ARRIVI
024500020218     C**!!!D18TBO        OREQ      'A'
024600020218     C**!!!TAMBAP        ANDEQ     'P'
024700941025     C* O ANCORA DA DECORRERE
024800020218     C**!!!TAMDDT        ORGT      D18DSP
024900941025     C* RILEGGO
025000020218     C*!N30TARKSC        READE     TNTAM000                               30
025100941025     C*
025200020218   X2C**!!!              ELSE
025300941024     C*
025400020218     C**!!!              MOVEL     TAMCTR        TARCTR
025500020218     C**!!!              MOVE      TAMPRG        TARPRG
025600980831     C                   MOVEL     'S'           WSICAR            1
025700941025     C*
025800941025     C* SE E' TARIFFA VALIDA --> ESCO
025900020218    3C**!!!D18DSP        IFGE      TAMDDT
026000020218     C**!!!D18DSP        ANDLE     TAMDST
026100020218     C**!!!              SETON                                        30
026200020218    3C**!!!              ENDIF
026300941024     C*
026400020218     C*!N30TARKSC        READE     TNTAM000                               30
026500020218    2C**!!!              ENDIF
026600020218    1C**!!!              ENDDO
026700941025     C*
026800941025     C                   ENDSR
