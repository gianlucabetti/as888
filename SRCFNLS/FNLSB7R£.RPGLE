000100930527     H DECEDIT('0,') DATEDIT(*DMY.)
000200941231     H* FNLSB7R *-----------------------------------------------------*
000300991130     H*  VARIAZIONI VOLUME E PESO CML BOLLE PARTENZA E TRANSITO -    *
000400930322     H*---------------------------------------------------------------*
000500930322     H* INDICATORI USATI                                              *
000600930322     H*---------------------------------------------------------------*
000700940613     H* 18 - OFF  FILE TASSAZIONE PADRONCINI
000800940613     H* 18 - ON   FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
000900941231     H* 30 - DI COMODO
001000941231     H* 31 - DI COMODO
001100930322     H*---------------------------------------------------------------*
001200930322     H* DATA BASE                                                     *
001300930322     H*---------------------------------------------------------------*
001400020613     F**!!!FNBLP36L  UF   E           K DISK
001500060801     f**   fnblp71L  uf   e           k disk
001600060801     ffnblp72L  uf   e           k disk
001700020626     F**!!!FNBTP36L  UF   E           K DISK
001800020626     ffnbtp16L  uf   e           k disk
001900941230     FFNBLT01L  IF   E           K DISK
002000020626     F**!!FNBTT01L  IF   E           K DISK
002100020626     ffnbtt11L  if   e           k disk
002200991103     FFIAR601L  IF   E           K DISK
002300011121     Ffiftt01L  IF   E           K DISK
002400011121     Ffiftd04L  UF   E           K DISK
002500020327     Ffiftd05L  UF   E           K DISK
002600020327     F                                     RENAME(fiftd000:fiftd005)
002700011121     Ffifst01L  IF   E           K DISK
002800011121     F                                     RENAME(fiftt000:fifst000)
002900011121     Ffifsd04L  UF   E           K DISK
003000011121     F                                     RENAME(fiftd000:fifsd000)
003100020327     Ffifsd05L  UF   E           K DISK
003200020327     F                                     RENAME(fiftd000:fifsd005)
003300930322     D*------------------
003400930322     D* PARAMETRI ESTERNI
003500930322     D*------------------
003600930322     D KPJBA         E DS
003700930324     D*------------------
003800930324     D* PARAMETRI INTERNI
003900930324     D*------------------
004000930324     D PARAM           DS
004100961008     D  PA1NUM                17     17  0
004200060801     D*            simfel
004300060801      * (14/06/02) da oggi serve x leggere solo le bolle partenza e    inal
004400060801      *            transito del terminal
004500961008     D  SIMFEL               158    160  0
004600940323     D*
004700941231     D* PASSAGGIO DATI A SVALORIZZAZ. DISTINTA PADRONC.  - FNLRE2R -
004800940323     D PARAM3          DS
004900940323     D  PA3PDR                 1      7  0
005000940323     D  PA3TSR                 8      8
005100940323     D  PA3NDC                 9     15  0
005200940323     D  PA3FCT                16     16
005300940323     D  PA3FVD                17     17
005400940323     D  PA3FVT                18     18
005500950113     D  PA3DDC                19     26  0
005600041122     D  PA3sml               256    256
005700941230     D*
005800941230     D* DS PER FNLV20R - DS DEI CAMPI PER CALCOLO VOLUME DA FATTURARE
005900941230     D DSLV20        E DS                  EXTNAME(FNLV20DS)
006000990826     D DSLV39        E DS                  EXTNAME(FNLV39DS)
006100020614
006200020614     d savlna          s                   like(blplna)
006300020614     d xx              s              2  0
006400940323     I*
006500930322     C*---------------------------------------------------------------*
006600930322     C*                    MAIN                                       *
006700930325     C* PROGRAMMA DI VARIAZIONE VOLUME CML BOLLE PARTENZA E TRANSITO  *
006800930322     C*---------------------------------------------------------------*
006900930322     C*
007000930322     C* DEFINIZIONI VARIABILI E KLIST
007100930322     C                   EXSR      DEFVAR
007200930323     C*
007300930323     C* MANUTENZIONE BOLLE IN PARTENZA
007400930323     C                   EXSR      PARTEN
007500930323     C*
007600930323     C* MANUTENZIONE BOLLE IN TRANSITO
007700930323     C                   EXSR      TRANSI
007800930414     C*
007900930323     C                   SETON                                        LR
008000930323     C*---------------------------------------------------------------*
008100930323     C*                    PARTEN                                     *
008200930323     C* ROUTINE DI MANUTENZIONE BOLLE IN PARTENZA                     *
008300930323     C*---------------------------------------------------------------*
008400930323     C     PARTEN        BEGSR
008500930323     C*
008600990826     C* LEGGO BOLLA
008700020613     C**!!!              READ      FNBLP36L                             9830
008800060801     c
008900060801     c     simfel        setll     fnblp72l
009000060801     c     simfel        reade     fnblp72l                             9830
009100930324     C*
009200930401IF  1C     *IN30         DOWEQ     *OFF
009300961008     C     *IN98         ANDEQ     *OFF
009400020614
009500990826     C     BLPNCR        COMP      99999                                  65
009600990826     C     BLPNCP        COMP      99999                                  66
009700941230     C                   CLEAR                   DSLV20
009800990826     C                   CLEAR                   DSLV39
009900941230     C                   Z-ADD     *ZEROS        TOTVUC            8 6
010000990826     C                   Z-ADD     *ZEROS        TOTPUC            8 3
010100930421     C*
010200941231     C* SALVO IL VOLUME DA FATTURARE
010300941231     C                   MOVEL     BLPVLF        SAVVLF
010400990826     C                   MOVEL     BLPPKF        SAVPKF
010500941231     C* SALVO IL VOLUME CML
010600941231     C                   MOVEL     BLPVLC        SAVVLC
010700990826     C                   MOVEL     BLPPKC        SAVPKC
010800930324     C*
010900930323     C* LEGGO I COLLI DELLA BOLLA
011000941230     C     KBLT          SETLL     FNBLT01L
011100941230     C     KBLT          READE     FNBLT01L                               31
011200930401DO  2C     *IN31         DOWEQ     *OFF
011300930323     C*
011400930323     C* CALCOLO IL VOLUME TOTALE DELLA BOLLA
011500990826     C   65              ADD       BLTVUC        TOTVUC
011600990826     C   66              ADD       BLTPUC        TOTPUC
011700930323     C*
011800930323     C* CALCOLO IL NUMERO DI COLLI RILEVATI
011900990826IF  3C   65BLTVUC        IFGT      *ZEROS
012000941230     C                   ADD       1             D20NCR
012100930323E   3C                   ENDIF
012200990826     C* CALCOLO IL NUMERO DI COLLI RILEVATI
012300990826IF  3C   66BLTPUC        IFGT      *ZEROS
012400990826     C                   ADD       1             D39NCP
012500990826E   3C                   ENDIF
012600930323     C*
012700941230     C     KBLT          READE     FNBLT01L                               31
012800930323E   2C                   ENDDO
012900930713     C*
013000990826    2C   65TOTVUC        IFGT      0
013100941230     C* IMPOSTO IL VOLUME CML
013200941230     C                   Z-ADD(H)  TOTVUC        D20VLU
013300941231     C     D20VLU        IFEQ      0
013400941231     C                   Z-ADD     0,001         D20VLU
013500941231     C                   ENDIF
013600941231     C*
013700941230     C*
013800941230     C* ELABORO IL VOLUME CML SOLO SE VOLUME DA FATTURARE NON E' GIA' "
013900941230     C                   MOVEL     'P'           D20TBO
014000941230     C                   MOVEL     'C'           D20TVL
014100941230     C                   MOVEL     BLPFVF        D20FVF
014200941230     C                   MOVEL     BLPVLF        D20VLF
014300941230     C                   MOVEL     BLPFVB        D20FVB
014400941230     C                   MOVEL     BLPVLB        D20VLB
014500941230     C                   MOVEL     BLPAAS        D20AAS
014600941230     C                   MOVEL     BLPLNP        D20LNP
014700941230     C                   MOVEL     BLPNRS        D20NRS
014800941230     C                   MOVEL     BLPNSP        D20NSP
014900941230     C                   MOVEL     BLPCBO        D20CBO
015000941230     C                   MOVEL     BLPNCL        D20NCL
015100990924     C     BLPFT2        IFNE      ' '
015200941231     C                   MOVEL     'S'           D20FTR
015300990924     C                   ENDIF
015400991103     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
015500991103     C     KBLT2         CHAIN     FIAR6000                           30
015600991103     C  N30              MOVEL     AR6DFT        D20DFT
015700991103     C                   MOVEL     BLPLNA        D20LNA
015800960927     C*
015900960927     C     BLPDBR        IFGT      *ZEROS
016000960927     C                   MOVE      BLPDBR        D20DTE
016100960927     C                   ELSE
016200960927     C                   MOVE      BLPMGS        D20DTE
016300960927     C                   MOVEL     BLPAAS        D20DTE
016400960927     C                   END
016500941230     C                   MOVEL     DSLV20        KPJBU
016600941230     C*
016700941230     C                   CALL      'FNLV20R'
016800941230     C                   PARM                    KPJBA
016900040429     c                   parm      'N'           pwstor            1
017000941230     C*
017100941230     C                   MOVEL     KPJBU         DSLV20
017200941230     C*
017300990826     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
017400941231    3C     D20ERR        IFNE      '1'
017500941230     C                   MOVEL     D20NCR        BLPNCR
017600941230     C                   MOVEL     D20VLU        BLPVLC
017700941230     C                   MOVEL     D20FVF        BLPFVF
017800941230     C                   MOVEL     D20VLF        BLPVLF
017900990826E   3C                   ENDIF
018000990826E   2C                   ENDIF
018100990826      *
018200990826     C* IMPOSTO IL PESO CML
018300990826      *
018400990826    2C   66TOTPUC        IFGT      0
018500990826     C                   Z-ADD(H)  TOTPUC        D39PLU
018600990826     C     D39PLU        IFEQ      0
018700990826     C                   Z-ADD     0,1           D39PLU
018800990826     C                   ENDIF
018900990826     C*
019000990826     C*
019100990826     C* ELABORO IL PESO CML SOLO SE PESO DA FATTURARE NON E' GIA' "
019200990826     C                   MOVEL     'P'           D39TBO
019300990826     C                   MOVEL     'C'           D39TPP
019400990826     C                   MOVEL     BLPPKF        D39PLF
019500990826     C                   MOVEL     BLPPKB        D39PLB
019600990826     C                   MOVEL     BLPAAS        D39AAS
019700990826     C                   MOVEL     BLPLNP        D39LNP
019800990826     C                   MOVEL     BLPNRS        D39NRS
019900990826     C                   MOVEL     BLPNSP        D39NSP
020000990826     C                   MOVEL     BLPCBO        D39CBO
020100990826     C                   MOVEL     BLPNCL        D39NCL
020200991130     C     BLPFT2        IFNE      ' '
020300990921     C                   MOVEL     'S'           D39FTR
020400991130     C                   ENDIF
020500991103     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
020600991103     C     KBLT2         CHAIN     FIAR6000                           30
020700991103     C  N30              MOVEL     AR6DFT        D39DFT
020800990826     C                   MOVEL     BLPLNA        D39LNA
020900990826     C*
021000990826     C     BLPDBR        IFGT      *ZEROS
021100990826     C                   MOVE      BLPDBR        D39DTE
021200990826     C                   ELSE
021300990826     C                   MOVE      BLPMGS        D39DTE
021400990826     C                   MOVEL     BLPAAS        D39DTE
021500990826     C                   END
021600990826     C                   MOVEL     DSLV39        KPJBU
021700990826     C*
021800990826     C                   CALL      'FNLV39R'
021900990826     C                   PARM                    KPJBA
022000990826     C*
022100990826     C                   MOVEL     KPJBU         DSLV39
022200990826     C*
022300990826     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
022400990826    3C     D39ERR        IFNE      '1'
022500990826     C                   MOVEL     D39NCP        BLPNCP
022600990826     C                   MOVEL     D39PLU        BLPPKC
022700990826     C                   MOVEL     D39PLF        BLPPKF
022800990826E   3C                   ENDIF
022900990826E   2C                   ENDIF
023000941231     C* AGGIORNO FILE TASSAZIONE PADRONCINI
023100990826     C   65BLPFT2        IFNE      ' '
023200941231     C                   EXSR      AGGTAS
023300941231E   3C                   ENDIF
023400941231     C*
023500941231     C                   UPDATE    FNBLP000
023600941230     C*
023700020613     C**!!!              READ      FNBLP36L                             9830
023800060801     c     simfel        reade     fnblp72l                             9830
023900930325E   1C                   ENDDO
024000961008     C**
024100961008     C* SE 98 ON --> RECORD ALLOCATO, RISOTTOMETTO AZIONE
024200961008     C     *IN98         IFEQ      *ON
024300961008     C     PA1NUM        ANDLT     3
024400961008     C                   ADD       1             PA1NUM
024500961008     C                   MOVEL     'LSB7'        KCOAZ
024600961008     C                   MOVEL     PARAM         KPJBU
024700961008     C                   CALL      'BCH10'
024800961008     C                   PARM                    KPJBA
024900961008     C                   MOVEL     'S'           WSOTTM            1
025000961008     C                   ENDIF
025100961008     C                   SETOFF                                       98
025200930331     C*
025300930323     C                   ENDSR
025400930323     C*---------------------------------------------------------------*
025500930323     C*                    TRANSI                                     *
025600930323     C* ROUTINE DI MANUTENZIONE BOLLE IN TRANSITO                     *
025700930323     C*---------------------------------------------------------------*
025800930323     C     TRANSI        BEGSR
025900930607     C*
026000930326     C* PULIZIA CAMPI
026100930325     C                   MOVEA     '0'           *IN
026200941230     C                   Z-ADD     *ZEROS        TOTVUC
026300990826     C                   Z-ADD     *ZEROS        TOTPUC
026400930326     C*
026500990826     C* LEGGO BOLLA
026600020626     c     simfel        setll     fnbtp16l
026700020626     C**!!!              READ      FNBTP36L                             9830
026800020626     c     simfel        reade     fnbtp16l                             9830
026900930326     C*
027000930401DO  1C     *IN30         DOWEQ     *OFF
027100941230     C                   CLEAR                   DSLV20
027200941230     C                   Z-ADD     *ZEROS        TOTVUC
027300990826     C                   CLEAR                   DSLV39
027400990826     C                   Z-ADD     *ZEROS        TOTPUC
027500990920     C     BTPNCR        COMP      99999                                  65
027600990920     C     BTPNCP        COMP      99999                                  66
027700930408     C*
027800930326     C* LEGGO I COLLI DELLA BOLLA
027900020626     C**!!!KBTT          SETLL     FNBTT01L
028000020626     C**!!!KBTT          READE     FNBTT01L                               31
028100020626     c     kbtt          setll     fnbtt11l
028200020626     c     kbtt          reade     fnbtt11l                               31
028300930401DO  2C     *IN31         DOWEQ     *OFF
028400930326     C*
028500990826     C* CALCOLO IL VOLUME/PESO TOTALE DELLA BOLLA
028600990826     C   65              ADD       BTTVUC        TOTVUC
028700990826     C   66              ADD       BTTPUC        TOTPUC
028800930326     C*
028900930326     C* CALCOLO IL NUMERO DI COLLI RILEVATI
029000990826IF  3C   65BTTVUC        IFGT      *ZEROS
029100941231     C                   ADD       1             D20NCR
029200930326E   3C                   ENDIF
029300990826     C* CALCOLO IL NUMERO DI COLLI RILEVATI
029400990826IF  3C   66BTTPUC        IFGT      *ZEROS
029500990826     C                   ADD       1             D39NCP
029600990826E   3C                   ENDIF
029700930326     C*
029800020626     C**!!!KBTT          READE     FNBTT01L                               31
029900020626     c     kbtt          reade     fnbtt11l                               31
030000930326E   2C                   ENDDO
030100930329     C*
030200990826    2C   65TOTVUC        IFGT      0
030300941230     C* IMPOSTO IL VOLUME CML
030400941230     C                   Z-ADD(H)  TOTVUC        D20VLU
030500941231     C     D20VLU        IFEQ      0
030600941231     C                   Z-ADD     0,001         D20VLU
030700941231     C                   ENDIF
030800941230     C*
030900941230     C                   MOVEL     'T'           D20TBO
031000941230     C                   MOVEL     'C'           D20TVL
031100941231     C                   MOVEL     'S'           D20FTR
031200941230     C                   MOVEL     BTPFVB        D20FVB
031300941230     C                   MOVEL     BTPVLB        D20VLB
031400941231     C                   MOVEL     BTPFVB        D20FVF
031500941231     C                   MOVEL     BTPVLB        D20VLF
031600941230     C                   MOVEL     BTPAAS        D20AAS
031700941230     C                   MOVEL     BTPLNP        D20LNP
031800941230     C                   MOVEL     BTPNRS        D20NRS
031900941230     C                   MOVEL     BTPNSP        D20NSP
032000941230     C                   MOVEL     BTPCBO        D20CBO
032100941230     C                   MOVEL     BTPNCL        D20NCL
032200950120     C                   MOVEL     BTPLNA        D20LNA
032300960927     C                   MOVE      BTPDBR        D20DTE
032400941230     C                   MOVEL     DSLV20        KPJBU
032500941230     C*
032600941230     C                   CALL      'FNLV20R'
032700941230     C                   PARM                    KPJBA
032800040429     c                   parm      'N'           pwstor            1
032900941230     C*
033000941230     C                   MOVEL     KPJBU         DSLV20
033100941230     C*
033200941230     C* SE NON C'E' ERRORE : AGGORNO LA BOLLA
033300941230    4C     D20ERR        IFNE      '1'
033400941230     C                   MOVEL     D20NCR        BTPNCR
033500941230     C                   MOVEL     D20VLU        BTPVLC
033600990826E   4C                   ENDIF
033700990826E   2C                   ENDIF
033800990826      *
033900990826     C* IMPOSTO IL PESO CML
034000990826    2C   66TOTPUC        IFGT      0
034100990826     C                   Z-ADD(H)  TOTPUC        D39PLU
034200990826     C     D39PLU        IFEQ      0
034300990826     C                   Z-ADD     0,1           D39PLU
034400990826     C                   ENDIF
034500990826     C*
034600990826     C                   MOVEL     'T'           D39TBO
034700990826     C                   MOVEL     'C'           D39TPP
034800990826     C                   MOVEL     'S'           D39FTR
034900990826     C                   MOVEL     BTPPKB        D39PLB
035000990826     C                   MOVEL     BTPPKB        D39PLF
035100990826     C                   MOVEL     BTPAAS        D39AAS
035200990826     C                   MOVEL     BTPLNP        D39LNP
035300990826     C                   MOVEL     BTPNRS        D39NRS
035400990826     C                   MOVEL     BTPNSP        D39NSP
035500990826     C                   MOVEL     BTPCBO        D39CBO
035600990826     C                   MOVEL     BTPNCL        D39NCL
035700990826     C                   MOVEL     BTPLNA        D39LNA
035800990826     C                   MOVE      BTPDBR        D39DTE
035900990826     C                   MOVEL     DSLV39        KPJBU
036000990826     C*
036100990826     C                   CALL      'FNLV39R'
036200990826     C                   PARM                    KPJBA
036300990826     C*
036400990826     C                   MOVEL     KPJBU         DSLV39
036500990826     C*
036600990826     C* SE NON C'E' ERRORE : AGGORNO LA BOLLA
036700990826    4C     D39ERR        IFNE      '1'
036800990826     C                   MOVEL     D39NCP        BTPNCP
036900990826     C                   MOVEL     D39PLU        BTPPKC
037000990826E   4C                   ENDIF
037100990826E   2C                   ENDIF
037200941231     C*
037300941231     C                   UPDATE    FNBTP000
037400930326     C*
037500020626     C**!!!              READ      FNBTP36L                             9830
037600020626     c     simfel        reade     fnbtp16l                             9830
037700930326E   1C                   ENDDO
037800961008     C*
037900961008     C* SE 98 ON --> RECORD ALLOCATO, RISOTTOMETTO AZIONE
038000961008     C     *IN98         IFEQ      *ON
038100961008     C     PA1NUM        ANDLT     3
038200961008     C     WSOTTM        ANDEQ     ' '
038300961008     C                   ADD       1             PA1NUM
038400961008     C                   MOVEL     'LSB7'        KCOAZ
038500961008     C                   MOVEL     PARAM         KPJBU
038600961008     C                   CALL      'BCH10'
038700961008     C                   PARM                    KPJBA
038800961008     C                   MOVEL     'S'           WSOTTM            1
038900961008     C                   ENDIF
039000961008     C                   SETOFF                                       98
039100930331     C*
039200930323     C                   ENDSR
039300940323     C*---------------------------------------------------------------*
039400940323     C*                    AGGTAS                                     *
039500940323     C* AGGIORNO FILE TASSAZIONE PADRONCINI                           *
039600940323     C*---------------------------------------------------------------*
039700940323     C     AGGTAS        BEGSR
039800940331     C*
039900940331     C****  VOLUME  ****
040000940331     C*
040100940331     C* SE NON E' VARIATO
040200941231    1C     BLPVLC        IFNE      SAVVLC
040300940323     C*
040400940323     C                   CLEAR                   WSOTV
040500940323     C                   CLEAR                   WADDV
040600940323     C*
040700940323     C* VOLUME PARZIALE "Z"
040800940323     C* WSOTV  ===>  VALORE VECCHIO DA SOTTRARRE
040900941231    3C     SAVVLC        IFGE      SAVVLF
041000941231     C                   Z-ADD     SAVVLC        WSOTV
041100940323   X3C                   ELSE
041200941231     C                   Z-ADD     SAVVLF        WSOTV
041300940323    3C                   ENDIF
041400940323     C*
041500940323     C* WADDV  ===>  VALORE NUOVO DA AGGIUNGERE
041600050111     C     BLPNCR        ifeq      BLPNCL
041700941230     C                   Z-ADD     BLPVLC        WADDV
041800940323     C*
041900940323   X3C                   ELSE
042000941230     C     BLPVLC        IFGE      BLPVLF
042100941230     C                   Z-ADD     BLPVLC        WADDV
042200940323     C                   ELSE
042300941230     C                   Z-ADD     BLPVLF        WADDV
042400940323     C                   ENDIF
042500940323    3C                   ENDIF
042600940323     C*
042700940323     C*
042800940331    3C     WSOTV         IFNE      WADDV
042900940323     C*
043000940613     C****
043100940613     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
043200011121     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
043300011121     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
043400940613     C****
043500940613     C* 18 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
043600940613     C                   SETOFF                                       18
043700940613     C*
043800950105     C****   F N F T T 0 0 F   ****
043900950105     C* IL NUMERO DISTINTA RITIRI E' COSI' COMPOSTO:
044000950105     C*   - FLAG PRESTAZIONE PADRONCINO: "M-MATTINO"    ---> 1000
044100950105     C*                                  "P-POMERIGGIO" ---> 5000
044200950105     C*   - AL NUMERO TROVATO VIENE ADDIZIONATO IL NUMERO RITIRO
044300950105     C                   CLEAR                   WNDC
044400950105     C     BLPFPP        IFEQ      'M'
044500950105     C                   Z-ADD     1000          WNDC
044600950105     C                   ELSE
044700950105     C                   Z-ADD     5000          WNDC
044800950105     C                   ENDIF
044900950105     C                   ADD       BLPNRT        WNDC
045000950120     C                   Z-ADD     BLPDRT        WDDC
045100950113     C* CODICE MITTENTE
045200950113     C     BLPCCM        IFGT      0
045300950113     C                   Z-ADD     BLPCCM        WKSC
045400950113     C                   ELSE
045500950113     C                   Z-ADD     BLPKSC        WKSC
045600950113     C                   ENDIF
045700950105     C*
045800011121     C  N18KFTT          CHAIN     fiftt000                           30
045900940614     C*
046000940331    4C     *IN30         IFEQ      *OFF
046100940323     C*
046200940614     C                   DO        2
046300011121     C   18KFTT          CHAIN     fifst000                           30
046400940614     C*
046500940614     C     *IN30         IFEQ      *OFF
046600940614     C*
046700940323     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
046800940331    5C     FTTFVL        IFNE      'C'
046900020327     C*
047000020327     C  N18KFTD5         CHAIN     fiftd005                           30
047100020327     C   18KFTD5         CHAIN     fifsd005                           30
047200020327     C* VOLUME
047300020327     c                   z-add     waddv         ftdvlu
047400020327     C*
047500020327     C  N30
047600020327     CANN18              UPDATE    fiftd005
047700020327     C  N30
047800020327     CAN 18              UPDATE    fifsd005
047900020327     c* se non trovato record su fiftd per spedizione significa che i dati
048000020327     c* sono memorizzati a modo vecchio (spedizioni raggruppate per cliente
048100020327     c* mittente): in questo caso richiamo routine per lettura e aggiornamen
048200020327     c* to dettaglio a modo vecchio
048300020327     c   30              exsr      aggftdold
048400940323     C*
048500940323     C*
048600940323     C* SE VALORIZZATA SVALORIZZO
048700940331    6C     FTTFVL        IFEQ      'V'
048800941231     C                   MOVEL     BLPPDR        PA3PDR
048900940323     C                   MOVEL     FTTTSR        PA3TSR
049000950113     C                   MOVEL     FTTDDC        PA3DDC
049100940323     C                   Z-ADD     FTTNDC        PA3NDC
049200940323     C                   MOVEL     'N'           PA3FCT
049300940323     C                   MOVEL     'N'           PA3FVD
049400940323     C                   MOVEL     'N'           PA3FVT
049500041122     C  n18              MOVEL     ' '           PA3sml
049600041122     C   18              MOVEL     'S'           PA3sml
049700940323     C                   MOVEL     PARAM3        KPJBU
049800011121     C  N18              CALL      'FICNE2R'
049900940614     C                   PARM                    KPJBA
050000011121     C   18              CALL      'FICNE9C'
050100940323     C                   PARM                    KPJBA
050200940331    6C                   ENDIF
050300940323     C*
050400940331    5C                   ENDIF
050500940614     C*
050600940614     C                   ENDIF
050700940614     C* 18 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
050800940614     C                   SETON                                        18
050900940614     C                   ENDDO
051000940614     C*
051100940331    4C                   ENDIF
051200940323    3C                   ENDIF
051300940323    1C                   ENDIF
051400940323     C*
051500940323     C                   ENDSR
051600020327     C*---------------------------------------------------------------*
051700020327     C*                    AGGFTDOLD                                  *
051800020327     C* Aggiornamento dettaglio tassaz. padroncini - Vecchia struttura*
051900020327     C*---------------------------------------------------------------*
052000020327     C     AGGFTDOLD     BEGSR
052100020327     C                   MOVE      WKSC          W0040             4 0
052200020327     C     W0040         IFEQ      9999
052300020327     C  N18KFTD          SETLL     fiftd000                               30
052400020327     C   18KFTD          SETLL     fifsd000                               30
052500020327     C     *IN30         IFEQ      *OFF
052600020327     C                   MOVE      8888          WKSC
052700020327     C                   END
052800020327     C                   END
052900020327     C****   F N F T D 0 0 F   ****
053000020327     C  N18KFTD          CHAIN     fiftd000                           30
053100020327     C   18KFTD          CHAIN     fifsd000                           30
053200020327     C*
053300020327     C* VOLUME
053400020327     C     FTDVLU        SUB       WSOTV         WVOL
053500020327     C     WVOL          ADD       WADDV         FTDVLU
053600020327     C*
053700020327     C  N30
053800020327     CANN18              UPDATE    fiftd000
053900020327     C  N30
054000020327     CAN 18              UPDATE    fifsd000
054100020327     c                   ENDSR
054200950105     C*---------------------------------------------------------------*
054300950105     C*                    DEFVAR                                     *
054400950105     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
054500950105     C*---------------------------------------------------------------*
054600950105     C     DEFVAR        BEGSR
054700950105     C*
054800950105     C     *ENTRY        PLIST
054900950105     C                   PARM                    KPJBA
055000950105     C                   MOVEL     KPJBU         PARAM
055100950105     C* TIPO PRESTAZIONE PADRONCINI
055200950105     C                   MOVEL     'R'           WTSR
055300950105     C*
055400991103     C* ACCESSO FNBLT01L/FIAR601L
055500950105     C     KBLT          KLIST
055600950105     C                   KFLD                    BLPAAS
055700950105     C                   KFLD                    BLPLNP
055800950105     C                   KFLD                    BLPNRS
055900950105     C                   KFLD                    BLPNSP
056000991103     C     KBLT2         KLIST
056100991103     C                   KFLD                    BLPAAS
056200991103     C                   KFLD                    BLPLNP
056300991103     C                   KFLD                    BLPNRS
056400991103     C                   KFLD                    BLPNSP
056500991103     C                   KFLD                    KTRC
056600950105     C* ACCESSO FNBTT01L
056700950105     C     KBTT          KLIST
056800020626     c                   kfld                    btpflp
056900950105     C                   KFLD                    BTPAAS
057000950105     C                   KFLD                    BTPLNP
057100950105     C                   KFLD                    BTPNRS
057200950105     C                   KFLD                    BTPNSP
057300950105     C* ACCESSO FILE TASSAZIONE PADRONCINI
057400950105     C     KFTD          KLIST
057500950105     C                   KFLD                    BLPPDR
057600950113     C                   KFLD                    WTSR
057700950105     C                   KFLD                    WNDC
057800950113     C                   KFLD                    WDDC
057900950113     C                   KFLD                    WKSC
058000950105     C                   KFLD                    BLPRSM
058100020327     C     KFTD5         KLIST
058200020327     C                   KFLD                    BLPPDR
058300020327     C                   KFLD                    WTSR
058400020327     C                   KFLD                    WNDC
058500020327     C                   KFLD                    WDDC
058600020327     C                   KFLD                    BLPAAS
058700020327     C                   KFLD                    BLPLNP
058800020327     C                   KFLD                    BLPNRS
058900020327     C                   KFLD                    BLPNSP
059000950105     C     KFTT          KLIST
059100950105     C                   KFLD                    BLPPDR
059200950105     C                   KFLD                    WTSR
059300950105     C                   KFLD                    WNDC
059400991103     C                   KFLD                    WDDC
059500991103     C*
059600991103     C                   MOVEL     1             KTRC
059700950105     C*
059800950105     C* DEFINIZIONE CAMPI
059900991103     C     *LIKE         DEFINE    AR6TRC        KTRC
060000991103     C     *LIKE         DEFINE    BLPVLF        SAVVLF
060100950105     C     *LIKE         DEFINE    BLPVLC        SAVVLC
060200990826     C     *LIKE         DEFINE    BLPPKF        SAVPKF
060300990826     C     *LIKE         DEFINE    BLPPKC        SAVPKC
060400950113     C     *LIKE         DEFINE    FTDKSC        WKSC
060500950113     C     *LIKE         DEFINE    FTDTSR        WTSR
060600950105     C     *LIKE         DEFINE    FTDNDC        WNDC
060700950113     C     *LIKE         DEFINE    FTDDDC        WDDC
060800950105     C     *LIKE         DEFINE    BLPVLF        WSOTV
060900950105     C     *LIKE         DEFINE    BLPVLF        WADDV
061000950105     C     *LIKE         DEFINE    FTDVLU        WVOL
061100060801     C                   ENDSR
