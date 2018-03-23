000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200950315      *---------*----------------------------------------------------*
000300950315      * FNLR63R2*                                                    *
000400950315      *---------*                                                    *
000500950315      *         - GESTIONE CHIUSURA DISTINTA ESTERO -                *
000600000426      *                                                              *
000700000426      *  Il programma prevede il caricamento a video degli errori da *
000800000426      *  visualizzare da un sfl nascosto. L'utente potrà decidere se *
000900000426      *  correggere gli errori, escludere la spedizione dalla distina*
001000000426      *  o non considerare l'informazione arrivataci con EDI (A=an-  *
001100000426      *  nullo la spedizione da CDE). Le modifiche rimangono sospese *
001200000426      *  fino a che l'utente non preme F6. Solo dopo tale conferma   *
001300000426      *  si procede alla lettura del sfl nascosto (precedetemente    *
001400000426      *  aggiornato in base alle scelte effettuate) alla modifica su *
001500000426      *  altri file e al ricaricamento degli eventuali errori rimasti*
001600000426      *  a video. Solo quando non ci saranno più errori da gestire   *
001700000426      *  il pgm restituirà il controllo al chiamante                 *
001800950315      *--------------------------------------------------------------*
001900000418     FFNLR63D   CF   E             WORKSTN SFILE(LR63S03:NRR3)
002000950317     F                                     SFILE(LR63S05:NRR5)
002100000418     FFNCDE02L  UF   E           K DISK    USROPN  COMMIT
002200000418     FFNARB01L  UF   E           K DISK    USROPN  COMMIT
002300000418     FFNCDS03L  UF   E           K DISK    USROPN  COMMIT
002400950320      *----------------------------------------------------------------*
002500950320      *    s c h i e r e                                               *
002600950320      *----------------------------------------------------------------*
002700980325     D ERR             S             70    DIM(7) CTDATA PERRCD(1)              ERRORI
002800950315      *--------------------------------------------------------------*
002900950315      *  DS
003000950315      *--------------------------------------------------------------*
003100950315      * DS per richiamo pgm FNLR36R
003200950315     D DSLR36          DS
003300950315     D  P36AAS                14     17  0
003400950315     D  P36LNP                18     20  0
003500950315     D  P36NRS                21     22  0
003600950315     D  P36NSP                23     29  0
003700950315     D  P36F03                30     30
003800950315     D  P36FLG                31     31
003900000418     D fnlr63ds      E DS
004000000418      *
004100950315     D KPJBA         E DS
004200000418      *
004300950315     D                SDS
004400950315     D  NOMPGM                 1     10
004500000418      *
004600000418     D Wnr3key         S                   LIKE(NR3key)
004700950315      *---------------------------------------------------------------*
004800950315      * FLUSSO PRINCIPALE                                             *
004900950315      *---------------------------------------------------------------*
005000950317     C     *ENTRY        PLIST
005100950317     C                   PARM                    KPJBA
005200000418     C                   MOVEL     KPJBU         fnlr63ds
005300950320     C*
005400950320     C     D63TRI        IFNE      'U'
005500950317     C*  Caricamento subfile errori da subfile hidden
005600950317     C                   EXSR      INZS03
005700950315      *  Loop gestione subfile degli errori
005800950315     C     WFINE         DOUEQ     'S'
005900950317     C*  Gestione subfile errori
006000950317     C     WTPVID        CASEQ     '3'           GESS03
006100950315     C                   END
006200950317     C                   END
006300950315     C*
006400950315     C     FINE          TAG
006500000418     C                   MOVEL     fnlr63ds      KPJBU
006600950315     C                   SETON                                        RT
006700980403     C                   Z-ADD     *ZEROS        TSTFRZ
006800950320     C                   ELSE
006900950320     C*
007000950320     C                   COMMIT
007100950320     C                   SETON                                        LR
007200950320     C                   END
007300950317      *---------------------------------------------------------------*
007400950317      * Gestione videata subfile errori                               *
007500950317      *---------------------------------------------------------------*
007600950317     C     INZS03        BEGSR
007700950317      *
007800950320     C                   SETOFF                                       202128
007900950320     C                   WRITE     LR63C03
008000950317     C                   SETON                                        2021
008100950317     C                   MOVEL     D63DLA        V3DLNA
008200950317     C                   Z-ADD     D63LNA        V3CLNA
008300950317     C                   Z-ADD     D63NDT        V3CNDT
008400950317     C                   Z-ADD     1             NRR5              4 0
008500950317     C                   Z-ADD     0             NRR3              4 0
008600970304     C                   MOVEL     'S'           WNOREC            1
008700950320     C                   Z-ADD     1             NR3KEY
008800950321     C                   MOVEL     'N'           WFINE
008900980324      *
009000950317     C     NRR5          CHAIN     LR63S05                            31
009100980324      *
009200950317     C     *IN31         DOWEQ     '0'
009300950606     C                   Z-ADD     D63NDT        KNDC
009400950606     C                   Z-ADD     V5CAAS        KAAS
009500950606     C                   Z-ADD     V5CLNP        KLNP
009600950606     C                   Z-ADD     V5CNSP        KNSP
009700950606     C                   Z-ADD     V5CNRS        KNRS
009800980324      *  se spedizione = vecchia e *IN03 on prendo ex scelta
009900961122     C     WSVAAS        IFEQ      V5CAAS
010000961122     C     WSVLNP        ANDEQ     V5CLNP
010100961122     C     WSVNRS        ANDEQ     V5CNRS
010200961122     C     WSVNSP        ANDEQ     V5CNSP
010300961122     C     V5CIN3        ANDEQ     '1'
010400000426     C* .. se ho + errori per la stesssa spedizione li visualizzo tutti
010500000426     C* in cascata. La scelta va effettuata sulla prima riga e vale per
010600000426     C* tutte le segnalazioni della stessa spedizione
010700961122     C                   MOVEL     WSVSCE        V5CSCE
010800961122     C                   ELSE
010900961122     C*  salvo vecchia scelta e vecchio numero spedizione
011000961122     C                   MOVEL     V5CSCE        WSVSCE            1
011100961122     C                   Z-ADD     V5CAAS        WSVAAS            4 0
011200961122     C                   Z-ADD     V5CLNP        WSVLNP            3 0
011300961122     C                   Z-ADD     V5CNRS        WSVNRS            2 0
011400961122     C                   Z-ADD     V5CNSP        WSVNSP            7 0
011500961122     C                   END
011600980324      *  Controllo se record con entrambe le date
011700961120     C                   SETOFF                                       405022
011800961121     C                   SETOFF                                       41
011900961120     C                   MOVEL     *BLANKS       V3CSCE
012000980325      *
012100000426      *  Se la spedizione aveva + di una data impostata:
012200000426      *  giacenza, lasciato avviso, consegna...
012300000426      *  ... verifico se ne è stata scelta una
012400961120     C     V5CERR        IFEQ      ERR(2)
012500980325      *  Spedizione giacente
012600961120     C     V5CTPD        IFEQ      'G'
012700961120     C     KCDE          CHAIN     FNCDE02L                           31
012800961120     C                   Z-ADD     0             CDEDCM
012900961121     C                   Z-ADD     0             CDEDLA
013000961120     C  N31              EXCEPT    AGGGIA
013100961120     C                   END
013200980325      *  Spedizione consegnata
013300961120     C     V5CTPD        IFEQ      'C'
013400961120     C     KCDE          CHAIN     FNCDE02L                           31
013500961120     C                   Z-ADD     0             CDEDAG
013600961121     C                   Z-ADD     0             CDEDLA
013700961121     C                   MOVEL     *BLANKS       CDECMC
013800961120     C  N31              EXCEPT    AGGCON
013900961120     C                   END
014000980325      *  Spedizione lasciato avviso
014100961121     C     V5CTPD        IFEQ      'A'
014200961121     C     KCDE          CHAIN     FNCDE02L                           31
014300961121     C                   Z-ADD     0             CDEDCM
014400961121     C                   Z-ADD     0             CDEDAG
014500990716     C     CDECMC        IFNE      'AV9'
014600961121     C                   MOVEL     'AVV'         CDECMC
014700990716     C                   END
014800961121     C  N31              EXCEPT    AGGAVV
014900961121     C                   END
015000961120     C                   SETON                                        5022
015100961120     C                   END
015200980325      *
015300980325      *  Spedizione con note EDI
015400961120     C     V5CFTX        IFEQ      'S'
015500961211     C                   SETON                                        415022
015600961120     C                   END
015700980331      *
015800980331      *  Gestione dettaglio segnacolli
015900980331     C     V5CSCE        IFEQ      'S'                                          -- 1
016000980331      *
016100980331     C                   Z-ADD     V5CAAS        D63AAS
016200980331     C                   Z-ADD     V5CLNP        D63LNP
016300980331     C                   Z-ADD     V5CNSP        D63NSP
016400980331     C                   Z-ADD     V5CNRS        D63NRS
016500980331     C                   Z-ADD     V5CDSE        D63DSE
016600980401      *
016700000418     C                   MOVEL     fnlr63ds      KPJBU
016800980331     C                   CALL      'FNLR63R5'
016900980331     C                   PARM                    KPJBA
017000000418     C                   MOVEL     KPJBU         fnlr63ds
017100980331      *
017200980331      *  Se ho elaborato il dettaglio segnacolli determino il tipo di scelta
017300980331      *  se non ci sono rcd di FNCDS  ==> A
017400980331      *  se ci sono rcd con distinta uguale a quella in elaborazione ==> F
017500980331      *  se non ci sono rcd con distinta uguale a quella in elaborazione ==> E
017600980331      *
017700980331     C                   MOVEL     *BLANKS       V5CSCE
017800980331     C     V5CDSE        IFNE      D63DSE
017900980331      *
018000980331     C     KARB          CHAIN     FNCDS000                           32
018100980331     C     *IN32         IFEQ      '1'
018200980331     C                   MOVEL     'A'           V5CSCE
018300980331     C                   ELSE
018400980331      *
018500980331     C     *IN32         DOWEQ     '0'
018600980331     C                   MOVEL     'E'           V5CSCE
018700980331     C     D63NDT        IFEQ      CDSNDC
018800980331     C                   MOVEL     'F'           V5CSCE
018900980331     C                   SETON                                        32
019000980403     C                   EXCEPT    NOAGG
019100980331     C                   END
019200980331     C  N32KARB          READE     FNCDS000                               32
019300980331     C                   END
019400980331      *
019500980331     C                   END
019600980401     C                   END
019700980331      *
019800980331      * Aggiorno subfile - test di dettaglio segnacolli e scelta -
019900980331     C                   Z-ADD     D63DSE        V5CDSE
020000980331     C                   UPDATE    LR63S05
020100980331     C                   END
020200980325      *
020300980325      *  Annulla record
020400980325      *
020500950606     C     V5CSCE        IFEQ      'A'
020600980325      *
020700980325     C     KCDE          DELETE    FNCDE02L                           31
020800980331     C     *IN31         DOWEQ     '0'
020900980331     C     KARB          DELETE    FNCDS000                           31
021000980325     C                   END
021100980331     C                   SETOFF                                       31
021200980325      *  Se l'errore era forzabile imposto sottraggo 1 al numero
021300980325      *  delle bolle in distinta e aggiorno FNARB azzerando il
021400980325      *  il numero distinta
021500961121     C     V5CBLO        IFNE      'S'
021600961211     C     KARB          CHAIN     FNARB01L                           32
021700961211     C  N32ARBNDC        IFNE      0
021800961211     C                   SUB       1             D63NRR
021900961120     C                   END
022000020902     C                   Z-ADD     0             ARBifp
022001020902     C                   Z-ADD     0             ARBNDC
022100961211     C  N32              EXCEPT    AGGARB
022200961211     C                   END
022300950606     C                   GOTO      NOWRT
022400950606     C                   END
022500980325      *
022600980325      *  Escludi record
022700980325      *
022800961120     C     V5CSCE        IFEQ      'E'
022900961120     C     KCDE          CHAIN     FNCDE02L                           31
023000980325      *
023100980325     C     *IN31         IFEQ      '0'
023200980325     C                   Z-ADD     CDENDC        §NDC
023300980325     C                   Z-ADD     0             CDENDC
023400980325     C                   EXCEPT    AGGCDE
023500980325      *
023600980331     C     KARB          CHAIN     FNCDS000                           32
023700980331     C     *IN32         DOWEQ     '0'
023800980325     C     CDSNDC        IFEQ      §NDC
023900980325     C                   Z-ADD     0             CDSNDC
024000980325     C                   EXCEPT    AGGCDS
024100980325     C                   END
024200980331     C     KARB          READE     FNCDS000                               32
024300980325     C                   END
024400980325     C                   END
024500980325      *
024600961120     C*  Se l'errore era forzabile imposto sottraggo 1 al numero
024700961120     C*  delle bolle in distinta e aggiorno FNARB azzerando il
024800961120     C*  il numero distinta
024900961211     C     V5CBLO        IFNE      'S'
025000961211     C     KARB          CHAIN     FNARB01L                           32
025100961211     C  N32ARBNDC        IFNE      0
025200961120     C                   SUB       1             D63NRR
025300961211     C                   END
025400020902     C                   Z-ADD     0             ARBifp
025401020902     C                   Z-ADD     0             ARBNDC
025500961120     C  N32              EXCEPT    AGGARB
025600961120     C                   END
025700961120     C                   GOTO      NOWRT
025800961120     C                   END
025900980325      *
026000980325      *  Forzo immissione in distinta
026100980325      *
026200961120     C     V5CSCE        IFEQ      'F'
026300980403     C                   Z-ADD     1             TSTFRZ            1 0
026400980401      *
026500980401      * Se esiste dettaglio segnacolli aggiorno FNCDE con i dati di FNCDS
026600980401      * per la conferma distinte
026700980401     C     V5CDSE        IFGT      *ZEROS
026800980401     C                   EXSR      AFNCDE
026900980401     C                   END
027000980401      *
027100961120     C                   GOTO      NOWRT
027200961120     C                   END
027300980325      *
027400000426      *  Riscrivo subfile errori da subfile nascosto se non è stato in
027500000426      *  qualche modo eliminato
027600950606     C                   MOVEL     *BLANKS       V3CTPD
027700961122     C                   MOVEL     V5CSCE        V3CSCE
027800961122     C                   MOVEL     V5CIN3        V3CIN3
027900961122     C                   MOVEL     V5CIN3        *IN03
028000950317     C                   MOVEL     V5CERR        V3CERR
028100980325     C                   Z-ADD     V5CDSE        V3CDSE
028200980325     C                   Z-ADD     V5CAAS        V3CAAS
028300950317     C                   Z-ADD     V5CLNP        V3CLNP
028400950317     C                   Z-ADD     V5CNRS        V3CNRS
028500950317     C                   Z-ADD     V5CNSP        V3CNSP
028600950317     C                   Z-ADD     V5CDCM        V3CDCM
028700950317     C                   Z-ADD     V5CDLA        V3CDLA
028800950317     C                   Z-ADD     V5CDRC        V3CDRC
028900950317     C                   Z-ADD     V5CGIA        V3CGIA
029000950317     C                   Z-ADD     V5CRMN        V3CRMN
029100961120     C                   MOVEL     V5CCCA        V3CCCA
029200000210     C                   MOVEL     V5CTC1        V3CTC1
029300961120     C                   MOVEL     V5CSTS        V3CSTS
029400961120     C                   MOVEL     V5CCOD        V3CCOD
029500961120     C                   MOVEL     V5DCOD        V3DCOD
029600961120     C                   MOVEL     V5CBLO        V3CBLO
029700961120     C                   MOVEL     V5CFTX        V3CFTX
029800950317     C                   Z-ADD     NRR5          V3CNR5
029900961120     C                   ADD       1             NRR3
030000970304     C                   MOVEL     'N'           WNOREC            1
030100950317     C                   WRITE     LR63S03
030200980325      *
030300950606     C     NOWRT         TAG
030400950317     C                   ADD       1             NRR5
030500950317     C     NRR5          CHAIN     LR63S05                            31
030600950317     C                   END
030700980325      *
030800980325      *  Se il subfile degli errori è vuoto esco da programma
030900950317     C     NRR3          IFEQ      0
031000961211     C     D63NRR        ANDNE     0
031100950317     C                   MOVEL     'S'           WFINE
031200950317     C                   END
031300980325      *
031400950317     C                   ENDSR
031500980401      *---------------------------------------------------------------*
031600980401      * Aggiorno FNCDE se immesso in distinta con dettaglio segnacolli
031700980401      *---------------------------------------------------------------*
031800980401     C     AFNCDE        BEGSR
031900950315      *
032000980401     C     KCDE          CHAIN     FNCDE02L                           31
032100980401      *
032200980401     C     *IN31         IFEQ      '0'
032300980401     C     KARB          CHAIN     FNCDS000                           32
032400980401      *
032500980401     C     *IN32         DOWEQ     '0'
032600980401     C     CDSNDC        IFEQ      CDENDC
032700980401     C                   EXSR      MUOVI
032800980401     C                   UPDATE    FNCDE000
032900980401     C                   SETON                                        32
033000980403     C                   EXCEPT    NOAGG
033100980401     C                   END
033200980401     C  N32KARB          READE     FNCDS000                               32
033300980401     C                   END
033400980401     C                   END
033500980401      *
033600980401     C                   ENDSR
033700980401      *---------------------------------------------------------------*
033800980401      * Muovo da FNCDS ad FNCDE per aggiornamento
033900980401      *---------------------------------------------------------------*
034000980401     C     MUOVI         BEGSR
034100980401      *
034200980401     C                   Z-ADD     CDSDCM        CDEDCM
034300980401     C                   Z-ADD     CDSHMC        CDEHMC
034400980401     C                   Z-ADD     CDSDLA        CDEDLA
034500980401     C                   Z-ADD     CDSDAG        CDEDAG
034600980401     C                   Z-ADD     CDSDRC        CDEDRC
034700980401     C                   MOVEL     CDSNO1        CDENO1
034800980401     C                   MOVEL     CDSNO2        CDENO2
034900980401     C                   MOVEL     CDSCMC        CDECMC
035000980401     C                   MOVEL     CDSCCA        CDECCA
035100000210     C                   MOVEL     CDSTC1        CDETC1
035200980401     C                   MOVEL     CDSERR        CDEERR
035300980401     C                   MOVEL     CDSSTS        CDESTS
035400980401     C                   MOVEL     CDSCOD        CDECOD
035500980401     C                   MOVEL     CDSMOT        CDEMOT
035600980401     C                   MOVEL     CDSFL1        CDEFL1
035700980401     C                   MOVEL     CDSFL2        CDEFL2
035800980401     C                   MOVEL     '1'           CDEFL3
035900980401      *
036000980401     C                   ENDSR
036100980401      *---------------------------------------------------------------*
036200980401      * Gestione videata subfile errori                               *
036300980401      *---------------------------------------------------------------*
036400980401     C     GESS03        BEGSR
036500980401      *
036600961120      * Controllo se devo inviare messaggio che non ci sono bolle
036700961120      * buone in distinta
036800980325      *
036900950315     C                   SETON                                        2021
037000970304     C     WNOREC        IFEQ      'S'
037100000418     C                   eval      *in20 = *off
037200970207     C                   END
037300000418      *
037400000418     C                   if        TSTfrz = *zeros  and  D63nrr = *zeros
037500000418     C                   EVAL      NRR3 = 1
037600000418     c                   endif
037700000418      *
037800000418     C                   SELECT
037900000418     C                   WHEN      *in28 = *on
038000000418     C                   WHEN      Wnr3key > *zeros
038100000418     C                   eval        NR3KEY = wNR3KEY
038200000418     C                   OTHER
038300000418     C                   eval        NR3KEY = 1
038400000418     c                   ENDSL
038500980325      *
038600950315     C                   WRITE     LR63Z03
038700950315     C                   EXFMT     LR63C03
038800980325      *
038900950320     C                   Z-ADD     1             NR3KEY
039000000418     C                   eval      *in28 = *off
039100000418     C                   clear                   WNR3KEY
039200980325      *  Fine Lavoro
039300950315     C     *INKC         IFEQ      '1'
039400950315     C                   MOVEL     'S'           WFINE
039500950315     C                   MOVEL     '1'           D63F03
039600950315     C                   GOTO      FINVD3
039700950315     C                   END
039800980325      *  Escludi tutte
039900961127     C   KA              EXSR      TUTTE
040000980325      *  Loop di controlli
040100950315     C                   EXSR      CTR03
040200980325      *  Se ci sono ancora dei record ricarico il subfile
040300950606     C  N28
040400950606     CAN KF              EXSR      INZS03
040500980325      *
040600950317     C     FINVD3        ENDSR
040700961127      *---------------------------------------------------------------*
040800961127      *  Escludi tutte                                                *
040900961127      *---------------------------------------------------------------*
041000961127     C     TUTTE         BEGSR
041100961127      *
041200961127     C                   MOVEL     'N'           WINTER            1
041300961127     C                   Z-ADD     1             NRR3
041400961127     C     NRR3          CHAIN     LR63S03                            32
041500980325      *
041600961127     C     *IN32         DOWEQ     '0'
041700961127     C                   SETOFF                                       414050
041800980325      *
041900980325      *  Se deve essere effettuata una scelta G/C/A/E visualizzo il campo
042000961127     C     V3CERR        IFEQ      ERR(2)
042100961127     C     V3CFTX        OREQ      'S'
042200961127     C                   SETON                                        50
042300961127     C                   END
042400980325      *  Se record con più date e lo escludo è obbligatoria una scelta
042500961127     C     V3CERR        IFEQ      ERR(2)
042600961127     C     V3CTPD        ANDNE     ' '
042700961127     C                   SETON                                        4028
042800961127     C                   Z-ADD     NRR3          NR3KEY
042900961127     C                   MOVEL     ERR(1)        $MSG
043000961127     C                   END
043100980325      *
043200980325      *  Aggiorno subfile forzando 'E' nella scelta
043300961127     C                   SETON                                        22
043400961127     C                   MOVEL     V3CIN3        *IN03
043500961127     C                   MOVEL     'E'           V3CSCE
043600961127     C                   UPDATE    LR63S03
043700961127     C                   SETOFF                                       22
043800980325      *
043900980325      *  Aggiorno subfile nascosto
044000961127     C     V3CNR5        CHAIN     LR63S05                            31
044100961127     C                   MOVEL     V3CSCE        V5CSCE
044200961127     C                   MOVEL     V3CTPD        V5CTPD
044300961127     C  N31              UPDATE    LR63S05
044400980325      *
044500980325      * Leggo RCD successivo
044600961127     C                   ADD       1             NRR3
044700961127     C     NRR3          CHAIN     LR63S03                            32
044800961127     C                   END
044900980325      *
045000961127     C                   COMMIT
045100980325      *
045200961127     C                   ENDSR
045300950315      *---------------------------------------------------------------*
045400950315      *  Loop di errori  subfile errori                               *
045500950315      *---------------------------------------------------------------*
045600950315     C     CTR03         BEGSR
045700950315      *
045800950317     C                   MOVEL     'N'           WINTER            1
045900950317     C                   READC     LR63S03                                32
046000980325      *
046100950317     C     *IN32         DOWEQ     '0'
046200950320     C     *IN28         ANDEQ     '0'
046300980325      *
046400961121     C                   SETOFF                                       414050
046500950606     C     V3CERR        IFEQ      ERR(2)
046600961120     C     V3CFTX        OREQ      'S'
046700950606     C                   SETON                                        50
046800950606     C                   END
046900980325      *  Controllo se devo richiamare la visualizzazione delle note EDI
047000961120     C     V3CTPD        IFEQ      'E'
047100961120     C     V3CFTX        IFEQ      'S'
047200961120     C                   Z-ADD     V3CAAS        D63AAS
047300961120     C                   Z-ADD     V3CLNP        D63LNP
047400961120     C                   Z-ADD     V3CNSP        D63NSP
047500961120     C                   Z-ADD     V3CNRS        D63NRS
047600000418     C                   MOVEL     fnlr63ds      KPJBU
047700961120     C                   CALL      'FNLR63R4'
047800961120     C                   PARM                    KPJBA
047900961120     C                   MOVEL     *BLANKS       V3CTPD
048000000418     C                   Z-ADD     NRR3          wNR3KEY
048100980325      *
048200961120     C                   ELSE
048300961120     C                   SETON                                        4128
048400961120     C                   Z-ADD     NRR3          NR3KEY
048500961120     C                   MOVEL     ERR(4)        $MSG
048600961120     C                   END
048700961120     C                   END
048800980325      *
048900980325      *  Se non ho + date posso scrivere solo 'E'
049000961121     C     V3CTPD        IFNE      'E'
049100961121     C     V3CTPD        ANDNE     ' '
049200961121     C     V3CERR        ANDNE     ERR(2)
049300961121     C                   SETON                                        4128
049400961121     C                   Z-ADD     NRR3          NR3KEY
049500961121     C                   MOVEL     ERR(5)        $MSG
049600961121     C                   END
049700980325      *
049800980325      *  Se forzo spedizione con + date devo prima sceglierne una data
049900961212     C     V3CTPD        IFEQ      'F'
050000961212     C     V3CTPD        ANDEQ     ' '
050100961212     C     V3CERR        ANDNE     ERR(2)
050200961212     C                   SETON                                        4128
050300961212     C                   Z-ADD     NRR3          NR3KEY
050400961212     C                   MOVEL     ERR(6)        $MSG
050500961212     C                   END
050600980325      *
050700980325      *  Interrogazione bolle
050800950317     C     V3CSCE        IFEQ      'V'
050900950317     C                   MOVEL     'S'           WINTER
051000950317     C                   MOVEL     '1'           P36FLG
051100950317     C                   Z-ADD     V3CAAS        P36AAS
051200950317     C                   Z-ADD     V3CLNP        P36LNP
051300950317     C                   Z-ADD     V3CNSP        P36NSP
051400950317     C                   Z-ADD     V3CNRS        P36NRS
051500950317     C                   MOVEL     DSLR36        KPJBU
051600950317     C                   CALL      'FNLR36R'
051700950317     C                   PARM                    KPJBA
051800950606     C                   MOVEL     ' '           V3CSCE
051900000418     C                   Z-ADD     NRR3          wNR3KEY
052000950317     C                   ELSE
052100980325      *
052200980325      *  Controllo se l'errore è forzabile oppure no !
052300961120     C     V3CBLO        IFEQ      'S'
052400961120     C     V3CSCE        ANDEQ     'F'
052500961120     C                   SETON                                        4028
052600961120     C                   Z-ADD     NRR3          NR3KEY
052700961120     C                   MOVEL     ERR(3)        $MSG
052800961120     C                   END
052900980325      *
053000980325      *  Se record con dettaglio segnacolli da analizzare non posso forzare
053100980325     C     V3CDSE        IFGE      2
053200980325     C     V3CSCE        ANDEQ     'F'
053300980325     C                   SETON                                        4028
053400980325     C                   Z-ADD     NRR3          NR3KEY
053500980325     C                   MOVEL     ERR(3)        $MSG
053600980325     C                   END
053700980325      *
053800980325      *  Se record con + date se lo escludo o lo forzo è obbligatorio scegliere
053900950606     C     V3CERR        IFEQ      ERR(2)
054000950606     C     V3CSCE        IFNE      'E'
054100961120     C     V3CSCE        ANDNE     'F'
054200961120     C     V3CTPD        ANDNE     ' '
054300950606     C                   SETON                                        4028
054400950606     C                   Z-ADD     NRR3          NR3KEY
054500950606     C                   MOVEL     ERR(1)        $MSG
054600950606     C                   END
054700950606     C                   ELSE
054800980325      *
054900950317     C*  Escludi record
055000961120     C***        V3CSCE    IFEQ 'E'
055100961120     C***        D63NRR    ANDEQ0
055200961120     C***                  SETON                     4028
055300961120     C***                  Z-ADDNRR3      NR3KEY
055400961120     C***                  MOVELERR,1     $MSG
055500961120     C***                  END
055600980325      *
055700980325      *  Gestione dettaglio segnacolli solo se esiste
055800980325     C     V3CSCE        IFEQ      'S'
055900980325     C     V3CDSE        ANDLT     2
056000980325     C                   SETON                                        4028
056100980325     C                   Z-ADD     NRR3          NR3KEY
056200980325     C                   MOVEL     ERR(7)        $MSG
056300980325     C                   END
056400980325      *
056500950606     C                   END
056600980325      *
056700950320     C                   END
056800980325      *
056900980325      *  Aggiorno subfile
057000950320     C                   SETON                                        22
057100980325     C                   MOVEL     V3CIN3        *IN03
057200950320     C                   UPDATE    LR63S03
057300950320     C                   SETOFF                                       22
057400980325      *
057500980325      *  Aggiorno subfile nascosto
057600950317     C     V3CNR5        CHAIN     LR63S05                            31
057700961122     C                   MOVEL     V3CSCE        V5CSCE
057800950606     C                   MOVEL     V3CTPD        V5CTPD
057900950317     C  N31              UPDATE    LR63S05
058000980325      *
058100950320     C  N28              READC     LR63S03                                32
058200950317     C                   END
058300980325      *
058400950317     C                   COMMIT
058500980325     C*  Se ho richiesto interogazione bolle senza selezionarne alcuna
058600950317     C*  richiamo pgm.
058700950317     C     *INKG         IFEQ      '1'
058800950317     C     WINTER        ANDEQ     'N'
058900950317     C                   MOVEL     ' '           P36FLG
059000950317     C                   Z-ADD     V3CAAS        P36AAS
059100950317     C                   Z-ADD     V3CLNP        P36LNP
059200950317     C                   Z-ADD     V3CNSP        P36NSP
059300950317     C                   Z-ADD     V3CNRS        P36NRS
059400950317     C                   MOVEL     DSLR36        KPJBU
059500950317     C                   CALL      'FNLR36R'
059600950317     C                   PARM                    KPJBA
059700950317     C                   END
059800950317     C*
059900950317     C                   ENDSR
060000950317     C*-----------------------------------------------------*
060100950317     C     *INZSR        BEGSR
060200980325      *
060300980325      * Apertura controllata dei files
060400950317     C                   OPEN      FNCDE02L                             16
060500961120     C                   OPEN      FNARB01L                             16
060600980331     C                   OPEN      FNCDS03L                             16
060700950317     C   16              GOTO      FINE                                         OPEN vado a fine
060800980325      *
060900980325      *  Definizione chiavi
061000950317     C     KCDE          KLIST
061100950317     C                   KFLD                    KNDC
061200950317     C                   KFLD                    KAAS
061300950317     C                   KFLD                    KLNP
061400950317     C                   KFLD                    KNRS
061500950317     C                   KFLD                    KNSP
061600961120     C     KARB          KLIST
061700961120     C                   KFLD                    KAAS
061800961120     C                   KFLD                    KLNP
061900961120     C                   KFLD                    KNRS
062000961120     C                   KFLD                    KNSP
062100980325      *
062200980325      *  Definizione delle variabili
062300950317     C     *LIKE         DEFINE    CDEAAS        KAAS
062400950317     C     *LIKE         DEFINE    CDELNP        KLNP
062500950317     C     *LIKE         DEFINE    CDENRS        KNRS
062600950317     C     *LIKE         DEFINE    CDENSP        KNSP
062700950317     C     *LIKE         DEFINE    CDENDC        KNDC
062800980325     C     *LIKE         DEFINE    CDENDC        §NDC
062900980325      *
063000980325      *  Inizializzazione variabili
063100950317     C                   MOVEL     'N'           WFINE             1
063200950317     C                   MOVEL     '3'           WTPVID            1
063300980325      *
063400950317     C                   ENDSR
063500980325      *----------------------------------------------------*
063600950317     OFNCDE000  E            AGGCDE
063700950317     O                       CDENDC
063800950606     OFNCDE000  E            AGGGIA
063900950606     O                       CDEDCM
064000961121     O                       CDEDLA
064100950606     OFNCDE000  E            AGGCON
064200950606     O                       CDEDAG
064300961121     O                       CDEDLA
064400961121     O                       CDECMC
064500961121     OFNCDE000  E            AGGAVV
064600961121     O                       CDEDAG
064700961121     O                       CDEDCM
064800961121     O                       CDECMC
064900961120     OFNARB000  E            AGGARB
065000020902     O                       ARBifp
065001020902     O                       ARBNDC
065100980325     OFNCDS000  E            AGGCDS
065200980325     O                       CDSNDC
065300980403     OFNCDS000  E            NOAGG
065400980325     C*----------------------------------------------------*
065500980325**
065600950320Opzione non valida
065700961121Immetti G=sped.giac./C=Conse./A=L.Avv.
065800961120Non è possibile forzare l'immissione della spedizione in distinta
065900961120Non ci sono note EDI da visualizzare
066000961121E' possibile visualizzare solo le note EDI
066100961212Obbligatorio immettere C/G/A
066200980325Non esiste dettaglio segnacolli da visualizzare
