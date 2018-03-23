000100890727     F*------------------------------------------------------------*
000200000000     F*                                                            *
000300000000     F*  PGM  AB010R  MANUTENZIONE ANAGRAFICA BANCHE ESTERE        *
000400000000     F*                                                            *
000500890727     F*------------------------------------------------------------*
000600890727     FCNC591D   CF   E             WORKSTN
000700000000     F                                     SFILE(S1:Y)
000800000000     F                                     SFILE(S2:$)
000900000000     FCNABI01L  UF A E           K DISK
001000000321     FTABEL00F  IF   E           K DISK
001100890727     F*
001200000000     D ER              S             60    DIM(5) CTDATA PERRCD(1)              ERRORI
001300900301     D AGZ             S              1    DIM(60)                              DESCRIZ. A
001400900301     D LOC             S              1    DIM(40)                              DESCRIZ. L
001500890727     D*
001600000000     D KPJBA         E DS
001700000000     I*
001800890727     C*----------------------------------------------------*
001900890727     C*  CHIAVI UTILIZZATE A PROGRAMMA                     *
002000890727     C*----------------------------------------------------*
002100890727     C*
002200890727     C     KEY           KLIST
002300890727     C                   KFLD                    ABIV
002400890727     C                   KFLD                    CABV
002500890727     C*
002600000321     C*-----CHIAVE FILE     TABEL00F
002700000321     C     KTAB          KLIST
002800000321     C                   KFLD                    PARKUT            1 0
002900000321     C                   KFLD                    TIPTAB            2
003000000321     C                   KFLD                    KEYTAB            8
003100000321     C                   Z-ADD     1             PARKUT
003200890727     C*----------------------------------------------------*
003300890727     C*
003400000000     C     *ENTRY        PLIST
003500000000     C                   PARM                    KPJBA
003600000000     C*
003700890727     C                   MOVEL     KNMUS         VKNMUS
003800890727     C*
003900890727     C*----------------------------------------------------*
004000890727     C*
004100890727     C*   EMISSIONE PRIMO FORMATO
004200000000     C*
004300000000     C                   MOVE      *BLANKS       ABIV
004400000000     C                   MOVE      *BLANKS       CABV
004500000000     C                   MOVE      *BLANKS       DESV1
004600000000     C                   MOVE      *BLANKS       DESV2
004700000000     C*
004800000000     C     PRIMO         TAG
004900000000     C*
005000000000     C                   EXFMT     F1
005100000000     C                   SETOFF                                       909192
005200900108     C                   SETOFF                                       1195
005300000000     C*
005400000320     C   KC              GOTO      FINE
005500000000     C*
005600000000     C     ABIV          IFEQ      *ZEROS
005700000000     C     CABV          ANDEQ     *ZEROS
005800000000     C                   EXSR      CARIC1
005900000000     C   90              GOTO      PRIMO
006000000000     C*
006100900108     C     ROLL          TAG
006200900108     C*
006300000000     C                   Z-ADD     0             CHOIC1
006400890727     C                   WRITE     V99
006500000000     C                   EXFMT     SC1
006600000000     C*
006700900108     C     *IN95         IFEQ      '1'
006800900108     C                   EXSR      CARIC1
006900900108     C                   GOTO      ROLL
007000900108     C                   END
007100900108     C*
007200000000     C     CHOIC1        IFEQ      *ZEROS
007300000000     C                   MOVE      *BLANKS       ABIV
007400000000     C                   MOVE      *BLANKS       CABV
007500000000     C                   MOVE      *BLANKS       DESV1
007600000000     C                   MOVE      *BLANKS       DESV2
007700000000     C                   GOTO      PRIMO
007800000000     C                   ELSE
007900000000     C                   Z-ADD     CHOIC1        Y
008000000000     C     Y             CHAIN     S1                                 44
008100000000     C  N44              MOVE      ABIABI        ABIV
008200000000     C  N44              MOVEL     DESCR1        DESV1
008300000000     C  N44              SETON                                        11
008400000000     C                   MOVE      *BLANKS       CABV
008500000000     C                   MOVE      *BLANKS       DESV2
008600000000     C                   GOTO      PRIMO
008700000000     C                   END
008800000000     C                   END
008900000000     C*
009000000000     C     ABIV          IFNE      *ZEROS
009100000000     C     CABV          ANDEQ     *ZEROS
009200000000     C                   EXSR      CARIC2
009300000000     C   90              GOTO      PRIMO
009400000000     C*
009500900108     C     ROLL2         TAG
009600900108     C*
009700000000     C                   Z-ADD     0             CHOIC2
009800890727     C                   WRITE     V99
009900890727     C                   EXFMT     SC2
010000000000     C*
010100900108     C     *IN95         IFEQ      '1'
010200900108     C                   EXSR      CARIC2
010300900108     C                   GOTO      ROLL2
010400900108     C                   END
010500900108     C*
010600000000     C     CHOIC2        IFEQ      *ZEROS
010700000000     C                   MOVE      *BLANKS       CABV
010800000000     C                   MOVE      *BLANKS       DESV2
010900000000     C                   ELSE
011000000000     C                   Z-ADD     CHOIC2        $
011100000000     C     $             CHAIN     S2                                 44
011200000000     C  N44              MOVE      ABICAB        CABV
011300000000     C  N44              MOVEL     DESCR2        DESV2
011400000000     C                   END
011500000000     C                   SETON                                        11
011600000000     C                   GOTO      PRIMO
011700000000     C                   END
011800000000     C*
011900890727     C*      EMISSIONE SECONDO FORMATO
012000000000     C*
012100000000     C                   SETOFF                                       222388
012200000000     C                   EXSR      CONTRO
012300000000     C*
012400000000     C     SEC           TAG
012500000000     C*
012600000000     C                   EXFMT     F2
012700000000     C                   SETOFF                                       99
012800000000     C*
012900000320     C   12              MOVE      *BLANKS       DESV1
013000000320     C   12              MOVE      *BLANKS       DESV2
013100000320     C   12              GOTO      PRIMO
013200000321     C* SE ABI>=9000 LO STATO DEVE ESSERE INDICATO,ALTRIMENTI A BLANK
013300000320     C     ABIV          IFGE      90000
013400000320     C     VSTATO        IFEQ      *BLANK
013500000320     C                   SETON                                        9991
013600000320     C                   MOVEL     ER(2)         ORRORE
013700000320     C                   GOTO      SEC
013800000320     C                   END
013900000320     C                   ELSE
014000000321     C     VSTATO        IFNE      *BLANK
014100000320     C                   SETON                                        9991
014200000320     C                   MOVEL     ER(4)         ORRORE
014300000320     C                   GOTO      SEC
014400000320     C                   END
014500000320     C                   END
014600000321      * CONTROLLO LO STATO
014700000321     C                   MOVEL     '15'          TIPTAB
014800000321     C                   MOVEL     *BLANKS       KEYTAB
014900000321     C                   MOVEL     VSTATO        KEYTAB
015000000321     C     KTAB          CHAIN     TABEL00F                           92
015100000321     C   92              SETON                                        99
015200000321     C   92              MOVEL     ER(5)         ORRORE
015300000321     C   92              GOTO      SEC
015400000320     C*
015500900601     C   05
015600000321     COR 16              DO
015700000000     C     KEY           CHAIN     CNABI01L                           99
015800000000     C   99              MOVEL     ER(3)         ORRORE
015900000000     C   99              GOTO      SEC
016000000321     C   16              MOVE      '*'           ABIANN
016100900601     C   05              MOVE      ' '           ABIANN
016200900601     C                   MOVE      ' '           ABIFTR
016300900601     C                   MOVE      *ZEROS        ABIDTR
016400900601     C                   EXCEPT    §ANN
016500000000     C                   MOVE      *BLANKS       DESV1
016600000000     C                   MOVE      *BLANKS       DESV2
016700000000     C                   GOTO      PRIMO
016800000000     C                   END
016900000000     C*
017000000000     C                   SETOFF                                       33
017100000000     C     KEY           CHAIN     CNABI01L                           33
017200000000     C*
017300000000     C                   MOVEL     ABVIST        ABIIST
017400000000     C                   MOVEL     ABVAGE        ABIAGE
017500000000     C                   MOVEL     ABVIND        ABIND
017600000000     C                   MOVEL     ABVLOC        ABILOC
017700000320      * ABICOM = I PRIMI 36 CTR SONO IL COMUNE E GLI ULTIMI 4 CTR SONO LO STATO
017800000000     C                   MOVEL     ABVCOM        ABICOM
017900000320     C                   MOVE      VSTATO        ABICOM
018000000000     C                   MOVE      ABVCAP        ABICAP
018100000000     C                   MOVE      ABVPRO        ABIPRO
018200900228     C                   MOVE      ' '           ABIFTR
018300900228     C                   MOVE      *ZEROS        ABIDTR
018400000000     C  N33              UPDATE    CNABI000
018500000000     C   33              MOVE      ABIV          ABIABI
018600000000     C   33              MOVE      CABV          ABICAB
018700000000     C   33              WRITE     CNABI000
018800000000     C                   MOVE      *BLANKS       DESV1
018900000000     C                   MOVE      *BLANKS       DESV2
019000000000     C                   GOTO      PRIMO
019100000000     C*
019200890727     C*----------------------------------------------------*
019300890727     C*
019400000000     C     FINE          TAG
019500000000     C                   SETON                                        LR
019600000000     C*
019700890727     C*----------------------------------------------------*
019800890727     C*
019900890727     C/EJECT
020000890727     C*
020100890727     C*----------------------------------------------------*
020200890727     C*-   CARIC1 =   CARICA SUB-FILE                     -*
020300890727     C*----------------------------------------------------*
020400890727     C*
020500000000     C     CARIC1        BEGSR
020600000000     C*
020700900108     C  N95              DO
020800000000     C                   SETON                                        55
020900000000     C                   WRITE     SC1
021000030320     C                   Z-ADD     0             Y                 4 0
021100000000     C                   Z-ADD     0             ABISAV            5 0
021200000000     C                   SETOFF                                       55
021300000000     C     *LOVAL        SETLL     CNABI01L
021400900108     C                   END
021500000000     C*
021600900108     C                   Z-ADD     0             CONTA
021700900108     C*
021800900108     C     CONTA         DOUEQ     14
021900000000     C                   READ      CNABI01L                               10
022000000000     C   10              GOTO      FUORI
022100000000     C*
022200030320     C     Y             CABEQ     9999          FUORI
022300900108     C*
022400000000     C     ABIABI        IFNE      ABISAV
022500000320     C******     ABIABI    OREQ 11111
022600000000     C                   MOVEL     ABIIST        DESCR1
022700000000     C                   ADD       1             Y
022800030320     C                   ADD       1             CONTA             4 0
022900000000     C                   WRITE     S1
023000000000     C                   END
023100000000     C                   MOVE      ABIABI        ABISAV
023200000000     C                   END
023300000000     C     FUORI         TAG
023400000000     C*
023500900108     C                   MOVE      Y             RCDNBR
023600900108     C*
023700000000     C     Y             IFEQ      0
023800000000     C                   MOVE      *BLANKS       CABV
023900000000     C                   MOVE      *BLANKS       CABV
024000000000     C                   MOVE      *BLANKS       DESV1
024100000000     C                   MOVE      *BLANKS       DESV2
024200000000     C                   SETON                                        90
024300000000     C                   MOVEL     ER(1)         ERRORE
024400000000     C                   END
024500000000     C*
024600000000     C                   ENDSR
024700890727     C*
024800890727     C*----------------------------------------------------*
024900890727     C*
025000890727     C/EJECT
025100890727     C*
025200890727     C*----------------------------------------------------*
025300890727     C*-   CARIC2 =   CARICA SUB-FILE                     -*
025400890727     C*----------------------------------------------------*
025500890727     C*
025600000000     C     CARIC2        BEGSR
025700000000     C*
025800900108     C  N95              DO
025900000000     C                   SETON                                        66
026000000000     C                   WRITE     SC2
026100030320     C                   Z-ADD     0             $                 4 0
026200000000     C                   SETOFF                                       66
026300000000     C     ABIV          SETLL     CNABI01L
026400900108     C                   END
026500000000     C*
026600900108     C                   DO        14
026700000000     C     ABIV          READE     CNABI01L                               10
026800000000     C   10              GOTO      OUT
026900000000     C*
027000000000     C                   MOVEL     ABIIST        DESV1
027100000000     C                   MOVEL     ABIIST        DSSC2
027200900301     C*                    MOVELABIAGE    DESCR2
027300900301     C                   EXSR      CREBNA
027400000000     C                   ADD       1             $
027500000000     C                   WRITE     S2
027600030320     C     $             CABEQ     9999          OUT
027700000000     C                   END
027800000000     C     OUT           TAG
027900000000     C*
028000900108     C                   MOVE      $             RCDNB1
028100900108     C*
028200000000     C     $             IFEQ      0
028300000000     C                   MOVE      *BLANKS       DESV1
028400000000     C                   MOVE      *BLANKS       DESV2
028500000000     C                   SETON                                        90
028600000000     C                   MOVEL     ER(1)         ERRORE
028700000000     C                   END
028800000000     C*
028900000000     C                   ENDSR
029000890727     C*
029100890727     C*----------------------------------------------------*
029200890727     C*
029300890727     C/EJECT
029400890727     C*
029500890727     C*----------------------------------------------------*
029600890727     C*-   CONTRO =   CONTROLLO                           -*
029700890727     C*----------------------------------------------------*
029800890727     C*
029900000000     C     CONTRO        BEGSR
030000000000     C*
030100000000     C     KEY           CHAIN     CNABI01L                           22
030200000000     C     *IN22         IFEQ      '0'
030300000000     C                   SETON                                        88
030400900601     C     ABIANN        COMP      '*'                                    85
030500000000     C                   MOVEL     ABIIST        ABVIST
030600000000     C                   MOVEL     ABIAGE        ABVAGE
030700000000     C                   MOVEL     ABIND         ABVIND
030800000000     C                   MOVEL     ABILOC        ABVLOC
030900000320      * ABICOM = I PRIMI 36 CTR SONO IL COMUNE E GLI ULTIMI 4 CTR SONO LO STATO
031000000000     C                   MOVEL     ABICOM        ABVCOM
031100000320     C                   MOVE      ABICOM        VSTATO
031200000000     C                   MOVE      ABICAP        ABVCAP
031300000000     C                   MOVE      ABIPRO        ABVPRO
031400000000     C                   ELSE
031500000000     C                   MOVE      *BLANKS       ABVIST
031600000000     C                   MOVE      *BLANKS       ABVAGE
031700000000     C                   MOVE      *BLANKS       ABVIND
031800000000     C                   MOVE      *BLANKS       ABVLOC
031900000000     C                   MOVE      *BLANKS       ABVCOM
032000000320     C                   MOVE      *BLANKS       VSTATO
032100000000     C                   MOVE      *ZEROS        ABVCAP
032200000000     C                   MOVE      *BLANKS       ABVPRO
032300000000     C     ABIV          CHAIN     CNABI01L                           23
032400000000     C  N23              MOVEL     ABIIST        ABVIST
032500000000     C  N23              SETON                                        88
032600000000     C                   END
032700960513     C*
032800960513     C*  SE CODICE ABI = 11111  (BANCA ESTERA)
032900960513     C*  LASCIO MODIFICARE DESCRIZIONE ISTITUTO
033000000320     C** 88      ABIV      IFEQ 11111
033100000320     C**                   SETOF                     88
033200000320     C**                   END
033300960513     C*
033400000000     C                   ENDSR
033500890727     C*
033600900301     C*-****************************************************
033700900301     C*- SE ABI E CAB TROVATI, CO METTO LA DESCRIZIONE IN BANCA APP.
033800900301     C*-****************************************************
033900900301     C     CREBNA        BEGSR
034000900301     C* PER L'AGENZIA PRENDO ANCHE LA LOCALITA' O IL COMUNE
034100900301     C     ABIAGE        IFEQ      *BLANKS
034200900301     C*
034300900301     C     ABILOC        IFEQ      *BLANKS
034400900301     C     ABICOM        IFEQ      *BLANKS
034500900301     C                   MOVEL     *BLANKS       DESCR2
034600900301     C                   ELSE
034700900301     C                   MOVEL     ABICOM        DESCR2
034800900301     C                   END
034900900301     C                   ELSE
035000900301     C                   MOVEL     ABILOC        DESCR2
035100900301     C                   END
035200900301     C*
035300900301     C                   ELSE
035400900301     C                   MOVEA     ABIAGE        AGZ
035500900301     C* INDIVIDUO IL PRIMO BLANKS DELLA DESCRIZIONE AGENZIA
035600900301     C                   Z-ADD     40            P                 2 0
035700900301     C     AGZ(P)        DOWEQ     ' '
035800900301     C                   SUB       1             P
035900900301     C                   END
036000900301     C                   ADD       2             P
036100900301     C* SE POSSO SCRIVERE ALMENO UN CARATTERE, RIEMPO LA DESCRIZ. AGENZ
036200900301     C*    CON LA LOCALITA PER LO SPAZIO RESTANTE
036300900301     C     P             IFLE      60
036400900301     C* SE LOCALITA O COMUNE IL BOANCO, NON SCRIVO PIUà NIENTE
036500900301     C     ABILOC        IFEQ      *BLANKS
036600900301     C     ABICOM        IFEQ      *BLANKS
036700900301     C                   GOTO      SOTTO
036800900301     C                   ELSE
036900900301     C                   MOVEA     ABICOM        LOC
037000900301     C                   END
037100900301     C                   ELSE
037200900301     C                   MOVEA     ABILOC        LOC
037300900301     C                   END
037400900301     C* RIEMPO AGE COL PARTE DO LOC
037500900301     C                   Z-ADD     1             Q                 2 0
037600900301     C     P             ADD       39            R                 3 0
037700900314     C*
037800900314     C     R             IFGT      60
037900900314     C                   Z-ADD     60            R
038000900314     C                   END
038100900301     C     P             DOWLE     R
038200900301     C                   MOVE      LOC(Q)        AGZ(P)
038300900301     C                   ADD       1             P
038400900301     C                   ADD       1             Q
038500900301     C                   END
038600900301     C*
038700900301     C     SOTTO         TAG
038800900301     C                   MOVEA     AGZ           DESCR2
038900900301     C                   END
039000900301     C                   END
039100030320     C*
039200030320     C                   EVAL      Descr2 = %TRIMR(Descr2) + ' ' + Abind
039300900301     C*
039400900301     C                   ENDSR
039500900601     OCNABI000  E            §ANN
039600900601     O                       ABIANN
039700900601     O                       ABIFTR
039800900601     O                       ABIDTR
039900000321**
040000000000Nessuna Agenzia legata al Codice ABI indicato !
040100000320Codice A.B.I. riservato a BANCHE ESTERE:imettere stato estero
040200000000Codice ABI/CAB non presente in Anagrafica !
040300000321Lo stato deve essere BLANK se ABI Italia
040400000321Stato errato
