000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200941025     H* FNLV18R  *---------------------------------------------------*
000300941024     H*     CALCOLO VOLUME AUTOMATICO                                *
000400921021     H*--------------------------------------------------------------*
000500941024     FTNTAM01L  IF   E           K DISK
000600940218      *
000700941025     D* DS PER FNLV18R - DS DEI CAMPI PER CALCOLO VOLUME AUTOMATICO
000800941025     D DSLV18        E DS                  EXTNAME(FNLV18DS)
000900941109     D  D18DSP                18     25  0
001000020218
001100160225     d trul27ds1     e ds
001200160225     d trulc7ds      e ds
001300160225     D DSTA01        E DS
001400160302     d WNTW            s                   like(o27ntw)
001500020218
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
003300160225     C                   CLEAR                   WFIE
003400160302     C                   CLEAR                   WNTW
003500941025     C*
003600941025     C* NON C'E' PROGRESSIVO, LO CERCO
003700941025    2C     D18PRG        IFEQ      *BLANKS
003800941024     C                   EXSR      TARCLI
003900941025     C                   ELSE
004000941024     C                   MOVEL     D18PRG        WPRG
004100941025    2C                   ENDIF
004200941025     C**
004300941025     C* CHAIN SU TARIFFA CLIENTE
004400941025     C**
004500941025    2C     WT8888        IFEQ      ' '
004600941024     C     KTAM          CHAIN     TNTAM000                           30
004700941025     C*
004800941024     C* SE NON C'E' RAPPORTO IN TARIFFA USO TARIFFA CARTELLO
004900941025    3C     *IN30         IFEQ      *ON
005000941024     C     TAMKPM        OREQ      *ZEROS
005100941025     C                   MOVEL     '1'           WT8888            1
005200160225     c
005300160225     c* se ho la tariffa --> prendo il network per cercare la cartello,
005400160225     c*  altrimenti NTW da bolla
005500160225    4c                   if        not *in30
005600160225     C                   MOVEL     TAMFLO        DSTA01
005700160225     c                   select
005800160225    5c                   when      §tadpd='S'
005900160225     c                   movel     'D'           wfie              1
006000160225    5c
006100160225     c                   when      §tafed='S'
006200160225     c                   movel     'F'           wfie              1
006300160225     c                   other
006400160225     c                   movel     tamfie        wfie
006500160225     c                   endsl
006600160225     c
006700160225    4c                   endif
006800160225     c
006900941025    3C                   ENDIF
007000941025    2C                   ENDIF
007100941025     C**
007200941025     C* CHAIN SU TARIFFA DI CATELLO
007300941025     C**
007400941025    2C     WT8888        IFNE      ' '
007500941025     C*
007600941025     C* CERCO IL CODICE TARIFFA DELLA TARIFFA DI CARTELLO
007700941025     C                   EXSR      TARCAR
007800941025     C*
007900941024     C                   MOVEL     88888         W0050
008000941025     C*
008100941025     C     WSICAR        IFEQ      'S'
008200941025     C     KCAR2         CHAIN     TNTAM000                           30
008300941025     C                   ELSE
008400941025     C                   SETON                                        30
008500941025     C                   ENDIF
008600941025    2C                   ENDIF
008700941024     C*
008800941024     C* IMPOSTO FLAG: "P" SE PRESUNTO CON TARIFFA CARTELLO (FRANCO)
008900941024     C*                   SE E' ASSEGNATO  SEMPRE
009000941024     C*               "K" SE IN AUTOMATICO CON TARIFFA DEL CLI. A CHILO
009100941024     C*               "C" SE IN AUTOMATICO CON TARIFFA DEL CLI. A COLLO
009200941025    2C  N30W0050         IFEQ      88888
009300941024     C     FLGTB1        OREQ      'A'
009400941108     C                   MOVEL     'P'           D18FVF
009500941024     C                   ELSE
009600941108     C                   MOVEL     TAMTIN        D18FVF
009700941025    2C                   ENDIF
009800941025     C*
009900941025     C*  30 ON - NON TROVATE TARIFFE--> VOLUME "P"AT
010000941108     C   30              MOVEL     'P'           D18FVF
010100941025     C**
010200941024     C*      RAPPORTO KG PER METRO CUBO
010300941025     C**
010400950114    2C     TAMKPM        IFNE      0
010500941024     C* VEDO SE A KG O A COLLO
010600941025    3C     TAMTIN        IFEQ      'K'
010700941108     C     D18PKF        DIV(H)    TAMKPM        D18VLF
010800941025    3C                   END
010900941025    3C     TAMTIN        IFEQ      'C'
011000941024     C                   MOVE      TAMKPM        COMMPC            5 3
011100941108     C     COMMPC        MULT      D18NCL        D18VLF
011200941025    3C                   END
011300941025    2C                   END
011400941025     C*
011500941025     C* SE  VOLUME=0 IMPOSTO 0,001
011600941108     C     D18VLF        IFEQ      0
011700941108     C                   Z-ADD     0,001         D18VLF
011800941024     C                   END
011900160225
012000160225    1C                   ENDIF
012100941025     C*
012200941025     C* CHIUSURA PGM
012300941025    1C     D18TLA        IFEQ      ' '
012400941025     C                   RETURN
012500160225   x1C                   ELSE
012600160225
012700160225    2c                   if        d18tla='C'
012800160225     c                   clear                   trulc7ds
012900160225     c                   eval      ic7tla='C'
013000160225     c                   call      'TRULC7R'
013100160225     c                   parm                    trulc7ds
013200160225     c
013300160225     c                   clear                   trul27ds1
013400160225     c                   eval      i27tla='C'
013500160229     c                   call      'TRUL27R1'
013600160225     c                   parm                    trul27ds1
013700160225    2c                   endif
013800160225
013900160225     C                   SETON                                        LR
014000160225     c
014100941025    1C                   ENDIF
014200941021     C*
014300941024     C* OPERAZIONI INIZIALI ------------------------------------------*
014400941024     C     *INZSR        BEGSR
014500941024     C     KTAM          KLIST
014600941024     C                   KFLD                    D18KSC
014700941024     C                   KFLD                    D18CTR
014800941024     C                   KFLD                    WPRG
014900941024     C     KTAM2         KLIST
015000941024     C                   KFLD                    D18KSC
015100941024     C                   KFLD                    D18CTR
015200941024     C* ACCESSO   TFTAM TARIFFA DI CARTELLO
015300941024     C     KCAR2         KLIST
015400941024     C                   KFLD                    TARKSC
015500941025     C                   KFLD                    TARCTR
015600941025     C                   KFLD                    TARPRG
015700941024     C**
015800941024     C* DEFINIZIONE CAMPI
015900941024     C**
016000941024     C     *LIKE         DEFINE    TAMPRG        WPRG
016100941025     C     *LIKE         DEFINE    TAMPRG        TARPRG
016200941025     C     *LIKE         DEFINE    TAMCTR        TARCTR
016300941025     C     *LIKE         DEFINE    TAMKSC        TARKSC
016400941025     C*
016500941024     C                   ENDSR
016600941024     C*
016700941024     C* CERCO LA TARIFFA DEL CLIENTE ---------------------------------*
016800941024     C     TARCLI        BEGSR
016900941025     C                   CLEAR                   W003A
017000071102     c* Per i codici 8888 o 9999 devo andared sulla cartello
017100071102     c                   move      d18ksc        w0040             4 0
017200071102     c
017300071102    0c                   if        w0040<>8888 and w0040<>9999
017400941025     C*
017500941024     C     KTAM2         SETLL     TNTAM000
017600941024     C     KTAM2         READE     TNTAM000                               30
017700941024     C*
017800941024    1C     *IN30         DOWEQ     *OFF
017900941024     C* SE ANNULLATA
018000941024    2C     TAMATB        IFEQ      'A'
018100941024     C* O BLOCCATA
018200160301     c
018300160301     c* ES 01/03/16: le bloccate non le escudo; e richiamo da manut bolle
018400160301     c*  di sede la tariffa va utilizzata anche se bloccata
018500160301     C***  TAMBAP        OREQ      'B'
018600160301     c
018700941024     C* O SOLO PER GLI ARRIVI ED E' BOLLA PARTENZA
018800941024     C     D18TBO        OREQ      'P'
018900941024     C     TAMBAP        ANDEQ     'A'
019000941024     C* O SOLO PER LE PARTENZE ED E' BOLLA ARRIVI
019100941024     C     D18TBO        OREQ      'A'
019200941024     C     TAMBAP        ANDEQ     'P'
019300941024     C* O ANCORA DA DECORRERE
019400941115     C     TAMDDT        ORGT      D18DSP
019500941024     C* RILEGGO
019600941024     C     KTAM2         READE     TNTAM000                               30
019700941025     C*
019800941025   X2C                   ELSE
019900941024     C*
020000941024     C* SE E' PROGRESSIVO VALIDO ESCO, ALTRIMENTI RILEGGO
020100941025     C                   MOVEL     TAMPRG        W003A             3
020200941024     C*
020300941115     C* SE TARIFFA SCADUTA LEGGO ALTRO PROGRESSIVO
020400941115     C* ALTRIMENTI ESCO DAL CICLO
020500941115    3C     TAMDST        IFLT      D18DSP
020600941024     C     KTAM2         READE     TNTAM000                               30
020700941115    3C                   ELSE
020800941115     C                   SETON                                        30
020900941025    3C                   ENDIF
021000941025    2C                   ENDIF
021100941025    1C                   ENDDO
021200071102    0C                   ENDif
021300941024     C*
021400941025     C* SE NON HO TROVATO NESSUN PROGRESSIVO PASSO SUBITO ALLA TARIFFA
021500941025     C* DI CARTELLO
021600941025     C     W003A         IFEQ      *BLANKS
021700941025     C                   MOVEL     '1'           WT8888
021800941025     C                   ELSE
021900941025     C                   CLEAR                   WT8888
022000941025     C                   MOVEL     W003A         WPRG
022100941025     C                   ENDIF
022200941025     C*
022300941024     C                   ENDSR
022400941024     C*
022500941024     C* CERCO LA TARIFFA DI CARTELLO ---------------------------------*
022600941024     C     TARCAR        BEGSR
022700160225     c                   clear                   wsicar
022800020218
022900160225      * Richiamo il trul27r1 per avere network bolla se non ho trovato
023000160225      *   la tariffa
023100160225     c                   if        wfie=' '
023200160225     c                   clear                   trul27ds1
023300021121     c                   Movel     D18Ctr        I27Ctb
023400020218     c                   eval      i27tla = d18tla
023500020218     c                   eval      i27tsp = d18tsp
023600020218     c                   eval      i27lna = d18lna
023700020308     c                   eval      i27tln = d18tln
023800020218     c                   eval      i27lnp = d18lnp
023900020312     c                   eval      i27cli = d18ksc
024000160225     c                   call      'TRUL27R1'
024100160225     c                   parm                    trul27ds1
024200160302     c                   movel     o27ntw        wntw
024300160225     c                   endif
024400160225
024500160302     c                   if        wfie <>' ' or wntw<>*blanks
024600160225     c                   clear                   trulc7ds
024700160225     c                   movel     wfie          ic7tfie
024800160302     c                   movel     wntw          ic7tntw
024900160225     c                   movel     d18dsp        ic7dta
025000160225     c                   Movel     D18Ctr        Ic7Ctb
025100160225     c                   call      'TRULC7R'
025200160225     c                   parm                    trulC7ds
025300160225     c
025400160225     c                   move      oc7kscc       tarksc
025500160225     c                   movel     oc7ctrc       tarctr
025600160225     c                   movel     oc7prgc       tarprg
025700160225     c
025800160225     c                   if        tarksc>0
025900980831     C                   MOVEL     'S'           WSICAR            1
026000160225     c                   endif
026100160225     c                   endif
026200941025     C*
026300941025     C                   ENDSR
