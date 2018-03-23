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
008900111118     c                   if        simfel>0
009000060801     c     simfel        setll     fnblp72l
009100060801     c     simfel        reade     fnblp72l                             9830
009200111118     c                   else
009300111118     c     *loval        setll     fnblp72l
009400111118     c                   read      fnblp72l                             9830
009500111118     c                   endif
009600930324     C*
009700930401IF  1C     *IN30         DOWEQ     *OFF
009800961008     C     *IN98         ANDEQ     *OFF
009900020614
010000990826     C     BLPNCR        COMP      99999                                  65
010100990826     C     BLPNCP        COMP      99999                                  66
010200941230     C                   CLEAR                   DSLV20
010300990826     C                   CLEAR                   DSLV39
010400941230     C                   Z-ADD     *ZEROS        TOTVUC            8 6
010500990826     C                   Z-ADD     *ZEROS        TOTPUC            8 3
010600930421     C*
010700941231     C* SALVO IL VOLUME DA FATTURARE
010800941231     C                   MOVEL     BLPVLF        SAVVLF
010900990826     C                   MOVEL     BLPPKF        SAVPKF
011000941231     C* SALVO IL VOLUME CML
011100941231     C                   MOVEL     BLPVLC        SAVVLC
011200990826     C                   MOVEL     BLPPKC        SAVPKC
011300930324     C*
011400930323     C* LEGGO I COLLI DELLA BOLLA
011500941230     C     KBLT          SETLL     FNBLT01L
011600941230     C     KBLT          READE     FNBLT01L                               31
011700930401DO  2C     *IN31         DOWEQ     *OFF
011800930323     C*
011900930323     C* CALCOLO IL VOLUME TOTALE DELLA BOLLA
012000990826     C   65              ADD       BLTVUC        TOTVUC
012100990826     C   66              ADD       BLTPUC        TOTPUC
012200930323     C*
012300930323     C* CALCOLO IL NUMERO DI COLLI RILEVATI
012400990826IF  3C   65BLTVUC        IFGT      *ZEROS
012500941230     C                   ADD       1             D20NCR
012600930323E   3C                   ENDIF
012700990826     C* CALCOLO IL NUMERO DI COLLI RILEVATI
012800990826IF  3C   66BLTPUC        IFGT      *ZEROS
012900990826     C                   ADD       1             D39NCP
013000990826E   3C                   ENDIF
013100930323     C*
013200941230     C     KBLT          READE     FNBLT01L                               31
013300930323E   2C                   ENDDO
013400930713     C*
013500990826    2C   65TOTVUC        IFGT      0
013600941230     C* IMPOSTO IL VOLUME CML
013700941230     C                   Z-ADD(H)  TOTVUC        D20VLU
013800941231     C     D20VLU        IFEQ      0
013900941231     C                   Z-ADD     0,001         D20VLU
014000941231     C                   ENDIF
014100941231     C*
014200941230     C*
014300941230     C* ELABORO IL VOLUME CML SOLO SE VOLUME DA FATTURARE NON E' GIA' "
014400941230     C                   MOVEL     'P'           D20TBO
014500941230     C                   MOVEL     'C'           D20TVL
014600941230     C                   MOVEL     BLPFVF        D20FVF
014700941230     C                   MOVEL     BLPVLF        D20VLF
014800941230     C                   MOVEL     BLPFVB        D20FVB
014900941230     C                   MOVEL     BLPVLB        D20VLB
015000941230     C                   MOVEL     BLPAAS        D20AAS
015100941230     C                   MOVEL     BLPLNP        D20LNP
015200941230     C                   MOVEL     BLPNRS        D20NRS
015300941230     C                   MOVEL     BLPNSP        D20NSP
015400941230     C                   MOVEL     BLPCBO        D20CBO
015500941230     C                   MOVEL     BLPNCL        D20NCL
015600990924     C     BLPFT2        IFNE      ' '
015700941231     C                   MOVEL     'S'           D20FTR
015800990924     C                   ENDIF
015900991103     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
016000991103     C     KBLT2         CHAIN     FIAR6000                           30
016100991103     C  N30              MOVEL     AR6DFT        D20DFT
016200991103     C                   MOVEL     BLPLNA        D20LNA
016300960927     C*
016400960927     C     BLPDBR        IFGT      *ZEROS
016500960927     C                   MOVE      BLPDBR        D20DTE
016600960927     C                   ELSE
016700960927     C                   MOVE      BLPMGS        D20DTE
016800960927     C                   MOVEL     BLPAAS        D20DTE
016900960927     C                   END
017000941230     C                   MOVEL     DSLV20        KPJBU
017100941230     C*
017200941230     C                   CALL      'FNLV20R'
017300941230     C                   PARM                    KPJBA
017400040429     c                   parm      'N'           pwstor            1
017500941230     C*
017600941230     C                   MOVEL     KPJBU         DSLV20
017700941230     C*
017800990826     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
017900941231    3C     D20ERR        IFNE      '1'
018000941230     C                   MOVEL     D20NCR        BLPNCR
018100941230     C                   MOVEL     D20VLU        BLPVLC
018200941230     C                   MOVEL     D20FVF        BLPFVF
018300941230     C                   MOVEL     D20VLF        BLPVLF
018400990826E   3C                   ENDIF
018500111118     C*
018600111118   X2C                   ELSE
018700111118     C* SE NON È PRESENTE NESSUN VOLUME CLEARO IL CAMPO NCR
018800111118     C                   CLEAR                   BLPNCR
018900990826E   2C                   ENDIF
019000990826      *
019100990826     C* IMPOSTO IL PESO CML
019200990826      *
019300990826    2C   66TOTPUC        IFGT      0
019400990826     C                   Z-ADD(H)  TOTPUC        D39PLU
019500990826     C     D39PLU        IFEQ      0
019600990826     C                   Z-ADD     0,1           D39PLU
019700990826     C                   ENDIF
019800990826     C*
019900990826     C*
020000990826     C* ELABORO IL PESO CML SOLO SE PESO DA FATTURARE NON E' GIA' "
020100990826     C                   MOVEL     'P'           D39TBO
020200990826     C                   MOVEL     'C'           D39TPP
020300990826     C                   MOVEL     BLPPKF        D39PLF
020400990826     C                   MOVEL     BLPPKB        D39PLB
020500990826     C                   MOVEL     BLPAAS        D39AAS
020600990826     C                   MOVEL     BLPLNP        D39LNP
020700990826     C                   MOVEL     BLPNRS        D39NRS
020800990826     C                   MOVEL     BLPNSP        D39NSP
020900990826     C                   MOVEL     BLPCBO        D39CBO
021000990826     C                   MOVEL     BLPNCL        D39NCL
021100991130     C     BLPFT2        IFNE      ' '
021200990921     C                   MOVEL     'S'           D39FTR
021300991130     C                   ENDIF
021400991103     C* CHAIN SU FIAR6 PER PRENDERE DATA FATTURA
021500991103     C     KBLT2         CHAIN     FIAR6000                           30
021600991103     C  N30              MOVEL     AR6DFT        D39DFT
021700990826     C                   MOVEL     BLPLNA        D39LNA
021800990826     C*
021900990826     C     BLPDBR        IFGT      *ZEROS
022000990826     C                   MOVE      BLPDBR        D39DTE
022100990826     C                   ELSE
022200990826     C                   MOVE      BLPMGS        D39DTE
022300990826     C                   MOVEL     BLPAAS        D39DTE
022400990826     C                   END
022500990826     C                   MOVEL     DSLV39        KPJBU
022600990826     C*
022700990826     C                   CALL      'FNLV39R'
022800990826     C                   PARM                    KPJBA
022900990826     C*
023000990826     C                   MOVEL     KPJBU         DSLV39
023100990826     C*
023200990826     C* SE NON C'E' ERRORE : AGGIORNO LA BOLLA
023300990826    3C     D39ERR        IFNE      '1'
023400990826     C                   MOVEL     D39NCP        BLPNCP
023500990826     C                   MOVEL     D39PLU        BLPPKC
023600990826     C                   MOVEL     D39PLF        BLPPKF
023700990826E   3C                   ENDIF
023800111118     C                   ELSE
023900111118     C* SE NON È PRESENTE NESSUN VOLUME CLEARO IL CAMPO NCR
024000111118     C                   CLEAR                   BLPNCP
024100990826E   2C                   ENDIF
024200111118     C
024300941231     C* AGGIORNO FILE TASSAZIONE PADRONCINI
024400990826     C   65BLPFT2        IFNE      ' '
024500941231     C                   EXSR      AGGTAS
024600941231E   3C                   ENDIF
024700941231     C*
024800941231     C                   UPDATE    FNBLP000
024900941230     C*
025000020613     C**!!!              READ      FNBLP36L                             9830
025100111118     c
025200111118     c                   if        simfel>0
025300060801     c     simfel        reade     fnblp72l                             9830
025400111118     c                   else
025500111118     c                   read      fnblp72l                             9830
025600111118     c                   endif
025700930325E   1C                   ENDDO
025800961008     C**
025900961008     C* SE 98 ON --> RECORD ALLOCATO, RISOTTOMETTO AZIONE
026000961008     C     *IN98         IFEQ      *ON
026100961008     C     PA1NUM        ANDLT     3
026200961008     C                   ADD       1             PA1NUM
026300961008     C                   MOVEL     'LSB7'        KCOAZ
026400961008     C                   MOVEL     PARAM         KPJBU
026500961008     C                   CALL      'BCH10'
026600961008     C                   PARM                    KPJBA
026700961008     C                   MOVEL     'S'           WSOTTM            1
026800961008     C                   ENDIF
026900961008     C                   SETOFF                                       98
027000930331     C*
027100930323     C                   ENDSR
027200930323     C*---------------------------------------------------------------*
027300930323     C*                    TRANSI                                     *
027400930323     C* ROUTINE DI MANUTENZIONE BOLLE IN TRANSITO                     *
027500930323     C*---------------------------------------------------------------*
027600930323     C     TRANSI        BEGSR
027700930607     C*
027800930326     C* PULIZIA CAMPI
027900930325     C                   MOVEA     '0'           *IN
028000941230     C                   Z-ADD     *ZEROS        TOTVUC
028100990826     C                   Z-ADD     *ZEROS        TOTPUC
028200930326     C*
028300990826     C* LEGGO BOLLA
028400111118     c                   if        simfel>0
028500020626     c     simfel        setll     fnbtp16l
028600111118     c                   else
028700111118     c     *loval        setll     fnbtp16l
028800111118     c                   endif
028900111118     c
029000111118     c                   if        simfel>0
029100020626     c     simfel        reade     fnbtp16l                             9830
029200111118     c                   else
029300111118     c                   read      fnbtp16l                             9830
029400111118     c                   endif
029500930326     C*
029600930401DO  1C     *IN30         DOWEQ     *OFF
029700941230     C                   CLEAR                   DSLV20
029800941230     C                   Z-ADD     *ZEROS        TOTVUC
029900990826     C                   CLEAR                   DSLV39
030000990826     C                   Z-ADD     *ZEROS        TOTPUC
030100990920     C     BTPNCR        COMP      99999                                  65
030200990920     C     BTPNCP        COMP      99999                                  66
030300930408     C*
030400930326     C* LEGGO I COLLI DELLA BOLLA
030500020626     C**!!!KBTT          SETLL     FNBTT01L
030600020626     C**!!!KBTT          READE     FNBTT01L                               31
030700020626     c     kbtt          setll     fnbtt11l
030800020626     c     kbtt          reade     fnbtt11l                               31
030900930401DO  2C     *IN31         DOWEQ     *OFF
031000930326     C*
031100990826     C* CALCOLO IL VOLUME/PESO TOTALE DELLA BOLLA
031200990826     C   65              ADD       BTTVUC        TOTVUC
031300990826     C   66              ADD       BTTPUC        TOTPUC
031400930326     C*
031500930326     C* CALCOLO IL NUMERO DI COLLI RILEVATI
031600990826IF  3C   65BTTVUC        IFGT      *ZEROS
031700941231     C                   ADD       1             D20NCR
031800930326E   3C                   ENDIF
031900990826     C* CALCOLO IL NUMERO DI COLLI RILEVATI
032000990826IF  3C   66BTTPUC        IFGT      *ZEROS
032100990826     C                   ADD       1             D39NCP
032200990826E   3C                   ENDIF
032300930326     C*
032400020626     C**!!!KBTT          READE     FNBTT01L                               31
032500020626     c     kbtt          reade     fnbtt11l                               31
032600930326E   2C                   ENDDO
032700930329     C*
032800990826    2C   65TOTVUC        IFGT      0
032900941230     C* IMPOSTO IL VOLUME CML
033000941230     C                   Z-ADD(H)  TOTVUC        D20VLU
033100941231     C     D20VLU        IFEQ      0
033200941231     C                   Z-ADD     0,001         D20VLU
033300941231     C                   ENDIF
033400941230     C*
033500941230     C                   MOVEL     'T'           D20TBO
033600941230     C                   MOVEL     'C'           D20TVL
033700941231     C                   MOVEL     'S'           D20FTR
033800941230     C                   MOVEL     BTPFVB        D20FVB
033900941230     C                   MOVEL     BTPVLB        D20VLB
034000941231     C                   MOVEL     BTPFVB        D20FVF
034100941231     C                   MOVEL     BTPVLB        D20VLF
034200941230     C                   MOVEL     BTPAAS        D20AAS
034300941230     C                   MOVEL     BTPLNP        D20LNP
034400941230     C                   MOVEL     BTPNRS        D20NRS
034500941230     C                   MOVEL     BTPNSP        D20NSP
034600941230     C                   MOVEL     BTPCBO        D20CBO
034700941230     C                   MOVEL     BTPNCL        D20NCL
034800950120     C                   MOVEL     BTPLNA        D20LNA
034900960927     C                   MOVE      BTPDBR        D20DTE
035000941230     C                   MOVEL     DSLV20        KPJBU
035100941230     C*
035200941230     C                   CALL      'FNLV20R'
035300941230     C                   PARM                    KPJBA
035400040429     c                   parm      'N'           pwstor            1
035500941230     C*
035600941230     C                   MOVEL     KPJBU         DSLV20
035700941230     C*
035800941230     C* SE NON C'E' ERRORE : AGGORNO LA BOLLA
035900941230    4C     D20ERR        IFNE      '1'
036000941230     C                   MOVEL     D20NCR        BTPNCR
036100941230     C                   MOVEL     D20VLU        BTPVLC
036200990826E   4C                   ENDIF
036300990826E   2C                   ENDIF
036400990826      *
036500990826     C* IMPOSTO IL PESO CML
036600990826    2C   66TOTPUC        IFGT      0
036700990826     C                   Z-ADD(H)  TOTPUC        D39PLU
036800990826     C     D39PLU        IFEQ      0
036900990826     C                   Z-ADD     0,1           D39PLU
037000990826     C                   ENDIF
037100990826     C*
037200990826     C                   MOVEL     'T'           D39TBO
037300990826     C                   MOVEL     'C'           D39TPP
037400990826     C                   MOVEL     'S'           D39FTR
037500990826     C                   MOVEL     BTPPKB        D39PLB
037600990826     C                   MOVEL     BTPPKB        D39PLF
037700990826     C                   MOVEL     BTPAAS        D39AAS
037800990826     C                   MOVEL     BTPLNP        D39LNP
037900990826     C                   MOVEL     BTPNRS        D39NRS
038000990826     C                   MOVEL     BTPNSP        D39NSP
038100990826     C                   MOVEL     BTPCBO        D39CBO
038200990826     C                   MOVEL     BTPNCL        D39NCL
038300990826     C                   MOVEL     BTPLNA        D39LNA
038400990826     C                   MOVE      BTPDBR        D39DTE
038500990826     C                   MOVEL     DSLV39        KPJBU
038600990826     C*
038700990826     C                   CALL      'FNLV39R'
038800990826     C                   PARM                    KPJBA
038900990826     C*
039000990826     C                   MOVEL     KPJBU         DSLV39
039100990826     C*
039200990826     C* SE NON C'E' ERRORE : AGGORNO LA BOLLA
039300990826    4C     D39ERR        IFNE      '1'
039400990826     C                   MOVEL     D39NCP        BTPNCP
039500990826     C                   MOVEL     D39PLU        BTPPKC
039600990826E   4C                   ENDIF
039700990826E   2C                   ENDIF
039800941231     C*
039900941231     C                   UPDATE    FNBTP000
040000930326     C*
040100111118     c                   if        simfel>0
040200020626     c     simfel        reade     fnbtp16l                             9830
040300111118     c                   else
040400111118     c                   read      fnbtp16l                             9830
040500111118     c                   endif
040600111118     c
040700930326E   1C                   ENDDO
040800961008     C*
040900961008     C* SE 98 ON --> RECORD ALLOCATO, RISOTTOMETTO AZIONE
041000961008     C     *IN98         IFEQ      *ON
041100961008     C     PA1NUM        ANDLT     3
041200961008     C     WSOTTM        ANDEQ     ' '
041300961008     C                   ADD       1             PA1NUM
041400961008     C                   MOVEL     'LSB7'        KCOAZ
041500961008     C                   MOVEL     PARAM         KPJBU
041600961008     C                   CALL      'BCH10'
041700961008     C                   PARM                    KPJBA
041800961008     C                   MOVEL     'S'           WSOTTM            1
041900961008     C                   ENDIF
042000961008     C                   SETOFF                                       98
042100930331     C*
042200930323     C                   ENDSR
042300940323     C*---------------------------------------------------------------*
042400940323     C*                    AGGTAS                                     *
042500940323     C* AGGIORNO FILE TASSAZIONE PADRONCINI                           *
042600940323     C*---------------------------------------------------------------*
042700940323     C     AGGTAS        BEGSR
042800940331     C*
042900940331     C****  VOLUME  ****
043000940331     C*
043100940331     C* SE NON E' VARIATO
043200941231    1C     BLPVLC        IFNE      SAVVLC
043300940323     C*
043400940323     C                   CLEAR                   WSOTV
043500940323     C                   CLEAR                   WADDV
043600940323     C*
043700940323     C* VOLUME PARZIALE "Z"
043800940323     C* WSOTV  ===>  VALORE VECCHIO DA SOTTRARRE
043900941231    3C     SAVVLC        IFGE      SAVVLF
044000941231     C                   Z-ADD     SAVVLC        WSOTV
044100940323   X3C                   ELSE
044200941231     C                   Z-ADD     SAVVLF        WSOTV
044300940323    3C                   ENDIF
044400940323     C*
044500940323     C* WADDV  ===>  VALORE NUOVO DA AGGIUNGERE
044600050111     C     BLPNCR        ifeq      BLPNCL
044700941230     C                   Z-ADD     BLPVLC        WADDV
044800940323     C*
044900940323   X3C                   ELSE
045000941230     C     BLPVLC        IFGE      BLPVLF
045100941230     C                   Z-ADD     BLPVLC        WADDV
045200940323     C                   ELSE
045300941230     C                   Z-ADD     BLPVLF        WADDV
045400940323     C                   ENDIF
045500940323    3C                   ENDIF
045600940323     C*
045700940323     C*
045800940331    3C     WSOTV         IFNE      WADDV
045900940323     C*
046000940613     C****
046100940613     C** ESEGUO L'AGGIORNAMENTO DEI FILE PADRONCINI 2 VOLTE:
046200011121     C**  - LA PRIMA   VOLTA PER I FILE fiftt00F/fiftd00F
046300011121     C**  - LA SECONDA VOLTA PER I FILE fifst00F/fifsd00F - SIMULAZIONE
046400940613     C****
046500940613     C* 18 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
046600940613     C                   SETOFF                                       18
046700940613     C*
046800950105     C****   F N F T T 0 0 F   ****
046900950105     C* IL NUMERO DISTINTA RITIRI E' COSI' COMPOSTO:
047000950105     C*   - FLAG PRESTAZIONE PADRONCINO: "M-MATTINO"    ---> 1000
047100950105     C*                                  "P-POMERIGGIO" ---> 5000
047200950105     C*   - AL NUMERO TROVATO VIENE ADDIZIONATO IL NUMERO RITIRO
047300950105     C                   CLEAR                   WNDC
047400950105     C     BLPFPP        IFEQ      'M'
047500950105     C                   Z-ADD     1000          WNDC
047600950105     C                   ELSE
047700950105     C                   Z-ADD     5000          WNDC
047800950105     C                   ENDIF
047900950105     C                   ADD       BLPNRT        WNDC
048000950120     C                   Z-ADD     BLPDRT        WDDC
048100950113     C* CODICE MITTENTE
048200950113     C     BLPCCM        IFGT      0
048300950113     C                   Z-ADD     BLPCCM        WKSC
048400950113     C                   ELSE
048500950113     C                   Z-ADD     BLPKSC        WKSC
048600950113     C                   ENDIF
048700950105     C*
048800011121     C  N18KFTT          CHAIN     fiftt000                           30
048900940614     C*
049000940331    4C     *IN30         IFEQ      *OFF
049100940323     C*
049200940614     C                   DO        2
049300011121     C   18KFTT          CHAIN     fifst000                           30
049400940614     C*
049500940614     C     *IN30         IFEQ      *OFF
049600940614     C*
049700940323     C* SE GIA' CONFERMATA NON AGGIORNO NIENTE
049800940331    5C     FTTFVL        IFNE      'C'
049900020327     C*
050000020327     C  N18KFTD5         CHAIN     fiftd005                           30
050100020327     C   18KFTD5         CHAIN     fifsd005                           30
050200020327     C* VOLUME
050300020327     c                   z-add     waddv         ftdvlu
050400020327     C*
050500020327     C  N30
050600020327     CANN18              UPDATE    fiftd005
050700020327     C  N30
050800020327     CAN 18              UPDATE    fifsd005
050900020327     c* se non trovato record su fiftd per spedizione significa che i dati
051000020327     c* sono memorizzati a modo vecchio (spedizioni raggruppate per cliente
051100020327     c* mittente): in questo caso richiamo routine per lettura e aggiornamen
051200020327     c* to dettaglio a modo vecchio
051300020327     c   30              exsr      aggftdold
051400940323     C*
051500940323     C*
051600940323     C* SE VALORIZZATA SVALORIZZO
051700940331    6C     FTTFVL        IFEQ      'V'
051800941231     C                   MOVEL     BLPPDR        PA3PDR
051900940323     C                   MOVEL     FTTTSR        PA3TSR
052000950113     C                   MOVEL     FTTDDC        PA3DDC
052100940323     C                   Z-ADD     FTTNDC        PA3NDC
052200940323     C                   MOVEL     'N'           PA3FCT
052300940323     C                   MOVEL     'N'           PA3FVD
052400940323     C                   MOVEL     'N'           PA3FVT
052500041122     C  n18              MOVEL     ' '           PA3sml
052600041122     C   18              MOVEL     'S'           PA3sml
052700940323     C                   MOVEL     PARAM3        KPJBU
052800011121     C  N18              CALL      'FICNE2R'
052900940614     C                   PARM                    KPJBA
053000011121     C   18              CALL      'FICNE9C'
053100940323     C                   PARM                    KPJBA
053200940331    6C                   ENDIF
053300940323     C*
053400940331    5C                   ENDIF
053500940614     C*
053600940614     C                   ENDIF
053700940614     C* 18 ON  - FILE TASSAZIONE PADRONCINI   S I M U L A Z I O N E
053800940614     C                   SETON                                        18
053900940614     C                   ENDDO
054000940614     C*
054100940331    4C                   ENDIF
054200940323    3C                   ENDIF
054300940323    1C                   ENDIF
054400940323     C*
054500940323     C                   ENDSR
054600020327     C*---------------------------------------------------------------*
054700020327     C*                    AGGFTDOLD                                  *
054800020327     C* Aggiornamento dettaglio tassaz. padroncini - Vecchia struttura*
054900020327     C*---------------------------------------------------------------*
055000020327     C     AGGFTDOLD     BEGSR
055100020327     C                   MOVE      WKSC          W0040             4 0
055200020327     C     W0040         IFEQ      9999
055300020327     C  N18KFTD          SETLL     fiftd000                               30
055400020327     C   18KFTD          SETLL     fifsd000                               30
055500020327     C     *IN30         IFEQ      *OFF
055600020327     C                   MOVE      8888          WKSC
055700020327     C                   END
055800020327     C                   END
055900020327     C****   F N F T D 0 0 F   ****
056000020327     C  N18KFTD          CHAIN     fiftd000                           30
056100020327     C   18KFTD          CHAIN     fifsd000                           30
056200020327     C*
056300020327     C* VOLUME
056400020327     C     FTDVLU        SUB       WSOTV         WVOL
056500020327     C     WVOL          ADD       WADDV         FTDVLU
056600020327     C*
056700020327     C  N30
056800020327     CANN18              UPDATE    fiftd000
056900020327     C  N30
057000020327     CAN 18              UPDATE    fifsd000
057100020327     c                   ENDSR
057200950105     C*---------------------------------------------------------------*
057300950105     C*                    DEFVAR                                     *
057400950105     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
057500950105     C*---------------------------------------------------------------*
057600950105     C     DEFVAR        BEGSR
057700950105     C*
057800950105     C     *ENTRY        PLIST
057900950105     C                   PARM                    KPJBA
058000950105     C                   MOVEL     KPJBU         PARAM
058100950105     C* TIPO PRESTAZIONE PADRONCINI
058200950105     C                   MOVEL     'R'           WTSR
058300950105     C*
058400991103     C* ACCESSO FNBLT01L/FIAR601L
058500950105     C     KBLT          KLIST
058600950105     C                   KFLD                    BLPAAS
058700950105     C                   KFLD                    BLPLNP
058800950105     C                   KFLD                    BLPNRS
058900950105     C                   KFLD                    BLPNSP
059000991103     C     KBLT2         KLIST
059100991103     C                   KFLD                    BLPAAS
059200991103     C                   KFLD                    BLPLNP
059300991103     C                   KFLD                    BLPNRS
059400991103     C                   KFLD                    BLPNSP
059500991103     C                   KFLD                    KTRC
059600950105     C* ACCESSO FNBTT01L
059700950105     C     KBTT          KLIST
059800020626     c                   kfld                    btpflp
059900950105     C                   KFLD                    BTPAAS
060000950105     C                   KFLD                    BTPLNP
060100950105     C                   KFLD                    BTPNRS
060200950105     C                   KFLD                    BTPNSP
060300950105     C* ACCESSO FILE TASSAZIONE PADRONCINI
060400950105     C     KFTD          KLIST
060500950105     C                   KFLD                    BLPPDR
060600950113     C                   KFLD                    WTSR
060700950105     C                   KFLD                    WNDC
060800950113     C                   KFLD                    WDDC
060900950113     C                   KFLD                    WKSC
061000950105     C                   KFLD                    BLPRSM
061100020327     C     KFTD5         KLIST
061200020327     C                   KFLD                    BLPPDR
061300020327     C                   KFLD                    WTSR
061400020327     C                   KFLD                    WNDC
061500020327     C                   KFLD                    WDDC
061600020327     C                   KFLD                    BLPAAS
061700020327     C                   KFLD                    BLPLNP
061800020327     C                   KFLD                    BLPNRS
061900020327     C                   KFLD                    BLPNSP
062000950105     C     KFTT          KLIST
062100950105     C                   KFLD                    BLPPDR
062200950105     C                   KFLD                    WTSR
062300950105     C                   KFLD                    WNDC
062400991103     C                   KFLD                    WDDC
062500991103     C*
062600991103     C                   MOVEL     1             KTRC
062700950105     C*
062800950105     C* DEFINIZIONE CAMPI
062900991103     C     *LIKE         DEFINE    AR6TRC        KTRC
063000991103     C     *LIKE         DEFINE    BLPVLF        SAVVLF
063100950105     C     *LIKE         DEFINE    BLPVLC        SAVVLC
063200990826     C     *LIKE         DEFINE    BLPPKF        SAVPKF
063300990826     C     *LIKE         DEFINE    BLPPKC        SAVPKC
063400950113     C     *LIKE         DEFINE    FTDKSC        WKSC
063500950113     C     *LIKE         DEFINE    FTDTSR        WTSR
063600950105     C     *LIKE         DEFINE    FTDNDC        WNDC
063700950113     C     *LIKE         DEFINE    FTDDDC        WDDC
063800950105     C     *LIKE         DEFINE    BLPVLF        WSOTV
063900950105     C     *LIKE         DEFINE    BLPVLF        WADDV
064000950105     C     *LIKE         DEFINE    FTDVLU        WVOL
064100060801     C                   ENDSR
