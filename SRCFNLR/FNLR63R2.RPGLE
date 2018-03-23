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
002100091007     F                                     SFILE(LR63s03X:NRX3)
002200000418     FFNCDE02L  UF   E           K DISK    USROPN  COMMIT
002300000418     FFNARB01L  UF   E           K DISK    USROPN  COMMIT
002400000418     FFNCDS03L  UF   E           K DISK    USROPN  COMMIT
002500950320      *----------------------------------------------------------------*
002600950320      *    s c h i e r e                                               *
002700950320      *----------------------------------------------------------------*
002800980325     D ERR             S             70    DIM(7) CTDATA PERRCD(1)              ERRORI
002900091007     D Doppi03         S              1    DIM(9999)                            Doppi su SFL
003000091007     D elementi        S              5s 0
003100950315      *--------------------------------------------------------------*
003200950315      *  DS
003300950315      *--------------------------------------------------------------*
003400950315      * DS per richiamo pgm FNLR36R
003500950315     D DSLR36          DS
003600950315     D  P36AAS                14     17  0
003700950315     D  P36LNP                18     20  0
003800950315     D  P36NRS                21     22  0
003900950315     D  P36NSP                23     29  0
004000950315     D  P36F03                30     30
004100950315     D  P36FLG                31     31
004200000418     D fnlr63ds      E DS
004300000418      *
004400950315     D KPJBA         E DS
004500000418      *
004600950315     D                SDS
004700950315     D  NOMPGM                 1     10
004800000418      *
004900000418     D Wnr3key         S                   LIKE(NR3key)
005000091005      **********************************************************************
005100091005      * Constants
005200091005      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
005300091005      **********************************************************************
005400091005      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
005500091006     D MaxKey          C                   5
005600091005     D Ascendente      C                   1
005700091005     D Discendente     C                   2
005800091005     D Carattere       C                   6
005900091005     D Put             C                   1
006000091005     D EndPut          C                   2
006100091005     D Get             C                   3
006200091005     D Numerico        C                   8
006300091005      **********************************************************************
006400091005      * Campi utili:
006500091005      *     SizeList   - Dimensione della lista
006600091005      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
006700091005      **********************************************************************
006800091005     D NotUsed         S             16A
006900091005     D ReturnSize      S              9B 0
007000091005     D SizeList        S              9B 0
007100091005     D RrnLast         S              5I 0
007200091005     D WrkSort         S              1  0 inz(0)
007300091005     D WindSort        S              3I 0 inz(0)
007400091005      **********************************************************************
007500091005      * Data Structures
007600091005      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
007700091005      *     QLGSCB     - The sort request block for the QLGSORT API
007800091005      *     QLGSCB00   - The sort request block for the QLGSRTIO API
007900091005      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
008000091005      *     QUSEC      - Error structure for the QLGSORT API
008100091005      **********************************************************************
008200091005     D SflRcd          DS
008300091005     D  V3CERR
008400091006     D  V3CAAS
008500091006     D  V3CLNP
008600091006     D  V3CNRS
008700091006     D  V3CNSP
008800091006     D  V3DCOD
008900091005     D  V3CNR5
009000091005     D  V3CBLO
009100091005     D  V3CFTX
009200091005     D  V3CFL1
009300091005     D  V3CIN3
009400091005     D  V3CDSE
009500091005     d  V3CSCE
009600091005     D  V3CRMN
009700091005     D  V3CTPD
009800091005     D  V3CDCM
009900091005     D  V3CDRC
010000091005     D  V3CDLA
010100091005     D  V3CGIA
010200091005     D  V3CCCA
010300091005     D  V3CTC1
010400091005     D  V3CSTS
010500091005     D  V3CCOD
010600091007     D  s03in03
010700091007     D  s03in22
010800091007     D  s03in40
010900091007     D  s03in41
011000091007     D  s03in50
011100091005     D  Selected                      1A
011200091005
011300091005      /COPY QSYSINC/QRPGLESRC,QLGSORT
011400091005     D QLGKL                         16    DIM(MaxKey)
011500091005     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
011600091005     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
011700091005     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
011800091005     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
011900091005
012000091005      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
012100091005      /COPY QSYSINC/QRPGLESRC,QUSEC
012200950315      *---------------------------------------------------------------*
012300950315      * FLUSSO PRINCIPALE                                             *
012400950315      *---------------------------------------------------------------*
012500950317     C     *ENTRY        PLIST
012600950317     C                   PARM                    KPJBA
012700000418     C                   MOVEL     KPJBU         fnlr63ds
012800950320     C*
012900950320     C     D63TRI        IFNE      'U'
013000950317     C*  Caricamento subfile errori da subfile hidden
013100950317     C                   EXSR      INZS03
013200950315      *  Loop gestione subfile degli errori
013300950315     C     WFINE         DOUEQ     'S'
013400950317     C*  Gestione subfile errori
013500950317     C     WTPVID        CASEQ     '3'           GESS03
013600950315     C                   END
013700950317     C                   END
013800950315     C*
013900950315     C     FINE          TAG
014000000418     C                   MOVEL     fnlr63ds      KPJBU
014100950315     C                   SETON                                        RT
014200980403     C                   Z-ADD     *ZEROS        TSTFRZ
014300950320     C                   ELSE
014400950320     C*
014500950320     C                   COMMIT
014600950320     C                   SETON                                        LR
014700950320     C                   END
014800950317      *---------------------------------------------------------------*
014900950317      * Gestione videata subfile errori                               *
015000950317      *---------------------------------------------------------------*
015100950317     C     INZS03        BEGSR
015200950317      *
015300950320     C                   SETOFF                                       202128
015400950320     C                   WRITE     LR63C03
015500950317     C                   SETON                                        2021
015600091007      *
015700091007     c                   clear                   Doppi03
015800091007      *
015900950317     C                   MOVEL     D63DLA        V3DLNA
016000950317     C                   Z-ADD     D63LNA        V3CLNA
016100950317     C                   Z-ADD     D63NDT        V3CNDT
016200950317     C                   Z-ADD     1             NRR5              4 0
016300950317     C                   Z-ADD     0             NRR3              4 0
016400091007     C                   Z-ADD     0             NRX3              4 0
016500970304     C                   MOVEL     'S'           WNOREC            1
016600950320     C                   Z-ADD     1             NR3KEY
016700950321     C                   MOVEL     'N'           WFINE
016800980324      *
016900950317     C     NRR5          CHAIN     LR63S05                            31
017000980324      *
017100950317     C     *IN31         DOWEQ     '0'
017200950606     C                   Z-ADD     D63NDT        KNDC
017300950606     C                   Z-ADD     V5CAAS        KAAS
017400950606     C                   Z-ADD     V5CLNP        KLNP
017500950606     C                   Z-ADD     V5CNSP        KNSP
017600950606     C                   Z-ADD     V5CNRS        KNRS
017700980324      *  se spedizione = vecchia e *IN03 on prendo ex scelta
017800961122     C     WSVAAS        IFEQ      V5CAAS
017900961122     C     WSVLNP        ANDEQ     V5CLNP
018000961122     C     WSVNRS        ANDEQ     V5CNRS
018100961122     C     WSVNSP        ANDEQ     V5CNSP
018200961122     C     V5CIN3        ANDEQ     '1'
018300000426     C* .. se ho + errori per la stesssa spedizione li visualizzo tutti
018400000426     C* in cascata. La scelta va effettuata sulla prima riga e vale per
018500000426     C* tutte le segnalazioni della stessa spedizione
018600961122     C                   MOVEL     WSVSCE        V5CSCE
018700961122     C                   ELSE
018800961122     C*  salvo vecchia scelta e vecchio numero spedizione
018900961122     C                   MOVEL     V5CSCE        WSVSCE            1
019000961122     C                   Z-ADD     V5CAAS        WSVAAS            4 0
019100961122     C                   Z-ADD     V5CLNP        WSVLNP            3 0
019200961122     C                   Z-ADD     V5CNRS        WSVNRS            2 0
019300961122     C                   Z-ADD     V5CNSP        WSVNSP            7 0
019400961122     C                   END
019500980324      *  Controllo se record con entrambe le date
019600961120     C                   SETOFF                                       405022
019700961121     C                   SETOFF                                       41
019800961120     C                   MOVEL     *BLANKS       V3CSCE
019900980325      *
020000000426      *  Se la spedizione aveva + di una data impostata:
020100000426      *  giacenza, lasciato avviso, consegna...
020200000426      *  ... verifico se ne è stata scelta una
020300961120     C     V5CERR        IFEQ      ERR(2)
020400980325      *  Spedizione giacente
020500961120     C     V5CTPD        IFEQ      'G'
020600961120     C     KCDE          CHAIN     FNCDE02L                           31
020700961120     C                   Z-ADD     0             CDEDCM
020800961121     C                   Z-ADD     0             CDEDLA
020900961120     C  N31              EXCEPT    AGGGIA
021000961120     C                   END
021100980325      *  Spedizione consegnata
021200961120     C     V5CTPD        IFEQ      'C'
021300961120     C     KCDE          CHAIN     FNCDE02L                           31
021400961120     C                   Z-ADD     0             CDEDAG
021500961121     C                   Z-ADD     0             CDEDLA
021600961121     C                   MOVEL     *BLANKS       CDECMC
021700961120     C  N31              EXCEPT    AGGCON
021800961120     C                   END
021900980325      *  Spedizione lasciato avviso
022000961121     C     V5CTPD        IFEQ      'A'
022100961121     C     KCDE          CHAIN     FNCDE02L                           31
022200961121     C                   Z-ADD     0             CDEDCM
022300961121     C                   Z-ADD     0             CDEDAG
022400990716     C     CDECMC        IFNE      'AV9'
022500961121     C                   MOVEL     'AVV'         CDECMC
022600990716     C                   END
022700961121     C  N31              EXCEPT    AGGAVV
022800961121     C                   END
022900961120     C                   SETON                                        5022
023000961120     C                   END
023100980325      *
023200980325      *  Spedizione con note EDI
023300961120     C     V5CFTX        IFEQ      'S'
023400961211     C                   SETON                                        415022
023500961120     C                   END
023600980331      *
023700980331      *  Gestione dettaglio segnacolli
023800980331     C     V5CSCE        IFEQ      'S'                                          -- 1
023900980331      *
024000980331     C                   Z-ADD     V5CAAS        D63AAS
024100980331     C                   Z-ADD     V5CLNP        D63LNP
024200980331     C                   Z-ADD     V5CNSP        D63NSP
024300980331     C                   Z-ADD     V5CNRS        D63NRS
024400980331     C                   Z-ADD     V5CDSE        D63DSE
024500980401      *
024600000418     C                   MOVEL     fnlr63ds      KPJBU
024700980331     C                   CALL      'FNLR63R5'
024800980331     C                   PARM                    KPJBA
024900000418     C                   MOVEL     KPJBU         fnlr63ds
025000980331      *
025100980331      *  Se ho elaborato il dettaglio segnacolli determino il tipo di scelta
025200980331      *  se non ci sono rcd di FNCDS  ==> A
025300980331      *  se ci sono rcd con distinta uguale a quella in elaborazione ==> F
025400980331      *  se non ci sono rcd con distinta uguale a quella in elaborazione ==> E
025500980331      *
025600980331     C                   MOVEL     *BLANKS       V5CSCE
025700980331     C     V5CDSE        IFNE      D63DSE
025800980331      *
025900980331     C     KARB          CHAIN     FNCDS000                           32
026000980331     C     *IN32         IFEQ      '1'
026100980331     C                   MOVEL     'A'           V5CSCE
026200980331     C                   ELSE
026300980331      *
026400980331     C     *IN32         DOWEQ     '0'
026500980331     C                   MOVEL     'E'           V5CSCE
026600980331     C     D63NDT        IFEQ      CDSNDC
026700980331     C                   MOVEL     'F'           V5CSCE
026800980331     C                   SETON                                        32
026900980403     C                   EXCEPT    NOAGG
027000980331     C                   END
027100980331     C  N32KARB          READE     FNCDS000                               32
027200980331     C                   END
027300980331      *
027400980331     C                   END
027500980401     C                   END
027600980331      *
027700980331      * Aggiorno subfile - test di dettaglio segnacolli e scelta -
027800980331     C                   Z-ADD     D63DSE        V5CDSE
027900980331     C                   UPDATE    LR63S05
028000980331     C                   END
028100980325      *
028200980325      *  Annulla record
028300980325      *
028400950606     C     V5CSCE        IFEQ      'A'
028500980325      *
028600980325     C     KCDE          DELETE    FNCDE02L                           31
028700980331     C     *IN31         DOWEQ     '0'
028800980331     C     KARB          DELETE    FNCDS000                           31
028900980325     C                   END
029000980331     C                   SETOFF                                       31
029100980325      *  Se l'errore era forzabile imposto sottraggo 1 al numero
029200980325      *  delle bolle in distinta e aggiorno FNARB azzerando il
029300980325      *  il numero distinta
029400961121     C     V5CBLO        IFNE      'S'
029500961211     C     KARB          CHAIN     FNARB01L                           32
029600961211     C  N32ARBNDC        IFNE      0
029700961211     C                   SUB       1             D63NRR
029800961120     C                   END
029900020902     C                   Z-ADD     0             ARBifp
030000020902     C                   Z-ADD     0             ARBNDC
030100961211     C  N32              EXCEPT    AGGARB
030200961211     C                   END
030300950606     C                   GOTO      NOWRT
030400950606     C                   END
030500980325      *
030600980325      *  Escludi record
030700980325      *
030800961120     C     V5CSCE        IFEQ      'E'
030900961120     C     KCDE          CHAIN     FNCDE02L                           31
031000980325      *
031100980325     C     *IN31         IFEQ      '0'
031200980325     C                   Z-ADD     CDENDC        §NDC
031300980325     C                   Z-ADD     0             CDENDC
031400980325     C                   EXCEPT    AGGCDE
031500980325      *
031600980331     C     KARB          CHAIN     FNCDS000                           32
031700980331     C     *IN32         DOWEQ     '0'
031800980325     C     CDSNDC        IFEQ      §NDC
031900980325     C                   Z-ADD     0             CDSNDC
032000980325     C                   EXCEPT    AGGCDS
032100980325     C                   END
032200980331     C     KARB          READE     FNCDS000                               32
032300980325     C                   END
032400980325     C                   END
032500980325      *
032600961120     C*  Se l'errore era forzabile imposto sottraggo 1 al numero
032700961120     C*  delle bolle in distinta e aggiorno FNARB azzerando il
032800961120     C*  il numero distinta
032900961211     C     V5CBLO        IFNE      'S'
033000961211     C     KARB          CHAIN     FNARB01L                           32
033100961211     C  N32ARBNDC        IFNE      0
033200961120     C                   SUB       1             D63NRR
033300961211     C                   END
033400020902     C                   Z-ADD     0             ARBifp
033500020902     C                   Z-ADD     0             ARBNDC
033600961120     C  N32              EXCEPT    AGGARB
033700961120     C                   END
033800961120     C                   GOTO      NOWRT
033900961120     C                   END
034000980325      *
034100980325      *  Forzo immissione in distinta
034200980325      *
034300961120     C     V5CSCE        IFEQ      'F'
034400980403     C                   Z-ADD     1             TSTFRZ            1 0
034500980401      *
034600980401      * Se esiste dettaglio segnacolli aggiorno FNCDE con i dati di FNCDS
034700980401      * per la conferma distinte
034800980401     C     V5CDSE        IFGT      *ZEROS
034900980401     C                   EXSR      AFNCDE
035000980401     C                   END
035100980401      *
035200961120     C                   GOTO      NOWRT
035300961120     C                   END
035400980325      *
035500000426      *  Riscrivo subfile errori da subfile nascosto se non è stato in
035600000426      *  qualche modo eliminato
035700950606     C                   MOVEL     *BLANKS       V3CTPD
035800961122     C                   MOVEL     V5CSCE        V3CSCE
035900961122     C                   MOVEL     V5CIN3        V3CIN3
036000961122     C                   MOVEL     V5CIN3        *IN03
036100950317     C                   MOVEL     V5CERR        V3CERR
036200980325     C                   Z-ADD     V5CDSE        V3CDSE
036300980325     C                   Z-ADD     V5CAAS        V3CAAS
036400950317     C                   Z-ADD     V5CLNP        V3CLNP
036500950317     C                   Z-ADD     V5CNRS        V3CNRS
036600950317     C                   Z-ADD     V5CNSP        V3CNSP
036700950317     C                   Z-ADD     V5CDCM        V3CDCM
036800950317     C                   Z-ADD     V5CDLA        V3CDLA
036900950317     C                   Z-ADD     V5CDRC        V3CDRC
037000950317     C                   Z-ADD     V5CGIA        V3CGIA
037100950317     C                   Z-ADD     V5CRMN        V3CRMN
037200961120     C                   MOVEL     V5CCCA        V3CCCA
037300000210     C                   MOVEL     V5CTC1        V3CTC1
037400961120     C                   MOVEL     V5CSTS        V3CSTS
037500961120     C                   MOVEL     V5CCOD        V3CCOD
037600961120     C                   MOVEL     V5DCOD        V3DCOD
037700961120     C                   MOVEL     V5CBLO        V3CBLO
037800961120     C                   MOVEL     V5CFTX        V3CFTX
037900950317     C                   Z-ADD     NRR5          V3CNR5
038000091007     c                   move      '0'           s03in03
038100091007     c                   move      '0'           s03in22
038200091007     c                   move      '0'           s03in40
038300091007     c                   move      '0'           s03in41
038400091007     c                   move      '0'           s03in50
038500091007      *
038600091007     c   03              move      '1'           s03in03
038700091007     c   22              move      '1'           s03in22
038800091007     c   40              move      '1'           s03in40
038900091007     c   41              move      '1'           s03in41
039000091007     c   50              move      '1'           s03in50
039100091007      *
039200961120     C                   ADD       1             NRR3
039300091007     c   03              move      *ON           Doppi03(nrr3)
039400091007      *
039500970304     C                   MOVEL     'N'           WNOREC            1
039600950317     C                   WRITE     LR63S03
039700980325      *
039800950606     C     NOWRT         TAG
039900950317     C                   ADD       1             NRR5
040000950317     C     NRR5          CHAIN     LR63S05                            31
040100950317     C                   END
040200980325      *
040300980325      *  Se il subfile degli errori è vuoto esco da programma
040400950317     C     NRR3          IFEQ      0
040500961211     C     D63NRR        ANDNE     0
040600950317     C                   MOVEL     'S'           WFINE
040700091007     C                   END
040800091007      *
040900091007     C     NRR3          IFgt      0
041000091007      *  Riordina il SFL x emissione seguendo la tipologia dell'errore
041100091007     c                   z-add     nrr3          rrnlast
041200980325      *
041300091007      *    prima flagga i record con doppie segnalazioni per la stessa bolla
041400091007      *   da escluderli durante l'ordinamento e da aggiungere in coda al SFL
041500091007      *   precedentemente ordinato.
041600091007     c                   eval      elementi = %Elem(Doppi03)
041700091007     c     1             do        elementi      Nrcd              5 0
041800091007     c                   if        Doppi03(Nrcd) = *ON
041900091007      * contrassegna anche quello precedente identico x stessa bolla
042000091007     c                   eval      Doppi03(Nrcd - 1) = *ON
042100091007      * questi records verranno esclusi dall'ordinamento
042200091007     c                   end
042300091007     c                   enddo
042400091007      *
042500091007      * si parcheggia sul SFL X03 le bolle con segnalazioni doppie
042600091007      *  per non farle riordinare
042700091007     c                   exsr      salva_Doppi
042800091007      *
042900091007      *  quindi riordina solo le bolle con una segnalazione singola di errore
043000091005     c                   exsr      Ordina_SF03
043100091007      *
043200091007      * in seguito le bolle doppie le accoda a quelle ordinate
043300091007     c                   exsr      accoda_Doppi
043400091007     C                   END
043500091005      *
043600950317     C                   ENDSR
043700091007      *---------------------------------------------------------------*
043800091007      *?  Si salva a parte su SFL X03 le bolle con segnalazioni       ?
043900091007      *?   multiple                                                   ?
044000091007      *---------------------------------------------------------------*
044100091007     C     Salva_Doppi   begSR
044200091007      *
044300091007      * sul SFL fittizio solo usato solo per il salvataggio di questi records
044400091007      * ed elimina il record da l SFL principale che dovrà essere riordinato
044500091007      *
044600091007     C                   SETOFF                                       6061
044700091007     C                   WRITE     LR63C03x
044800091007     C                   SETON                                        6061
044900091007      *
045000091007     c                   clear                   nrx3
045100091007     C*
045200091007     c     1             do        elementi      Nrcd              5 0
045300091007      *
045400091007      * quelli flaggati li salva
045500091007     c                   if        Doppi03(Nrcd) = *ON
045600091007     c     nrcd          chain     LR63S03
045700091007     c                   eval      X3CNR5   =  v3CNR5
045800091007     c                   eval      X3CBLO   =  v3CBLO
045900091007     c                   eval      X3CFTX   =  v3CFTX
046000091007     c                   eval      X3CFL1   =  v3CFL1
046100091007     c                   eval      X3CIN3   =  v3CIN3
046200091007     c                   eval      X3CDSE   =  v3CDSE
046300091007     c                   eval      X03IN03  =  s03IN03
046400091007     c                   eval      X03IN40  =  s03IN40
046500091007     c                   eval      X03IN41  =  s03IN41
046600091007     c                   eval      X03IN50  =  s03IN50
046700091007     c                   eval      X3CSCE   =  v3CSCE
046800091007     c                   eval      X3CAAS   =  v3CAAS
046900091007     c                   eval      X3CLNP   =  v3CLNP
047000091007     c                   eval      X3CNRS   =  v3CNRS
047100091007     c                   eval      X3CNSP   =  v3CNSP
047200091007     c                   eval      X3CRMN   =  v3CRMN
047300091007     c                   eval      X3CERR   =  v3CERR
047400091007     c                   eval      X3CTPD   =  v3CTPD
047500091007     c                   eval      X3CDCM   =  v3CDCM
047600091007     c                   eval      X3CDRC   =  v3CDRC
047700091007     c                   eval      X3CDLA   =  v3CDLA
047800091007     c                   eval      X3CGIA   =  v3CGIA
047900091007     c                   eval      X3CCCA   =  v3CCCA
048000091007     c                   eval      X3CTC1   =  v3CTC1
048100091007     c                   eval      X3CSTS   =  v3CSTS
048200091007     c                   eval      X3CCOD   =  v3CCOD
048300091007     c                   eval      X3DCOD   =  v3DCOD
048400091007     C                   ADD       1             NRX3
048500091007     C                   WRITE     LR63S03X
048600091007      *
048700091007      * PULISCE IL RECORD poichè lo ha trasferito
048800091007     c                   clear                   LR63S03
048900091007     C                   update    LR63S03
049000091007      *
049100091007     c                   end
049200091007      *
049300091007     c                   enddo
049400091007      *
049500091007     C     *like         define    NRX3          sav_nrx3
049600091007     C                   z-add     NRX3          sav_nrx3
049700091007      *
049800091007     C                   ENDSR
049900091007      *---------------------------------------------------------------*
050000091007      *? Riordina comunque il SFL                                     ?
050100091007      *?   Deve mostrare gli errori ordinati per descrizione          ?
050200091007      *---------------------------------------------------------------*
050300091007     C     Ordina_SF03   begSR
050400091007     C*
050500091007      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
050600091007      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
050700091007      * Il campo è usato per selezionare i records dando un ordine a quelli
050800091007      * selezionati davanti ai non selezionati.
050900091007     C                   CLEAR                   QLGSCB
051000091007     C                   CLEAR                   QLGSCB00
051100091007     C                   CLEAR                   NRR3
051200091007      *
051300091007     ** 5 campi chiave.
051400091007     C                   EVAL      QLGNBRK    = 5
051500091007      *
051600091007     C                   EVAL      QLGSP      = 1
051700091007     C                   EVAL      QLGSS      = %SIZE(v3cerr)
051800091007     C                   EVAL      QLGDT      = Carattere
051900091007     C                   EVAL      QLGSO      = Ascendente
052000091007     C                   EVAL      QLGKL(1)   = QLGSKL
052100091007      *
052200091007     C                   EVAL      QLGSP   = 1 + %SIZE(v3cerr)
052300091007     C                   EVAL      QLGSS    = %SIZE(v3caas)
052400091007     C                   EVAL      QLGDT    = Carattere
052500091007     C                   EVAL      QLGSO    = Ascendente
052600091007     C                   EVAL      QLGKL(2) = QLGSKL
052700091007      *
052800091007     C                   EVAL      QLGSP   = 1 + %SIZE(v3cerr)
052900091007     C                             +  %SIZE(v3caas)
053000091007     C                   EVAL      QLGSS    = %SIZE(v3clnp)
053100091007     C                   EVAL      QLGDT    = Carattere
053200091007     C                   EVAL      QLGSO    = Ascendente
053300091007     C                   EVAL      QLGKL(3) = QLGSKL
053400091007      *
053500091007     C                   EVAL      QLGSP   = 1 + %SIZE(v3cerr)
053600091007     C                             + %SIZE(v3caas) + %SIZE(v3clnp)
053700091007     C                   EVAL      QLGSS    = %SIZE(v3cnrs)
053800091007     C                   EVAL      QLGDT    = Carattere
053900091007     C                   EVAL      QLGSO    = Ascendente
054000091007     C                   EVAL      QLGKL(4) = QLGSKL
054100091007      *
054200091007     C                   EVAL      QLGSP   = 1 + %SIZE(v3cerr)
054300091007     C                             + %SIZE(v3caas) + %SIZE(v3clnp)
054400091007     c                             + %SIZE(v3cnrs)
054500091007     C                   EVAL      QLGSS    = %SIZE(v3cnsp)
054600091007     C                   EVAL      QLGDT    = Carattere
054700091007     C                   EVAL      QLGSO    = Ascendente
054800091007     C                   EVAL      QLGKL(5) = QLGSKL
054900091007      *
055000091007      *------------
055100091007      * Load other sort parameters.
055200091007     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
055300091007     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
055400091007     C                   EVAL      QLGRT     = 8
055500091007     C                   EVAL      QLGOKL    = 80
055600091007     C                   EVAL      QLGLKE    = 16
055700091007     C                   EVAL      QLGLSS    = 290
055800091007
055900091007      * Initialize Sort I/O API fields.
056000091007     C                   EVAL      QLGRL00  = QLGRL
056100091007     C                   EVAL      QLGRC00  = 1
056200091007     C                   CLEAR                   QUSEI
056300091007     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
056400091007
056500091007      * First step - Initialize the sort routine.
056600091007     C                   CALL      'QLGSORT'
056700091007     C                   PARM                    QLGSCB
056800091007     C                   PARM                    NotUsed
056900091007     C                   PARM                    NotUsed
057000091007     C                   PARM                    SizeList
057100091007     C                   PARM                    ReturnSize
057200091007     C                   PARM                    QUSEC
057300091007
057400091007      * Next step - Write records to I/O routine.
057500091007     C                   EVAL      QLGRT00 = Put
057600091007
057700091007     C                   DO        RrnLast       S3nrr_xx          5 0
057800091007     C     S3nrr_xx      CHAIN     Lr63S03
057900091007
058000091007     ** Solo le righe con Selected = 'Y' sono riordinate,
058100091007     ** quindi per fare un ordinamento di tutte le righe
058200091007     ** metto 'Y' sempre.
058300091007     C                   EVAL      Selected  = 'Y'
058400091007
058500091007     C                   CLEAR                   QUSEI
058600091007     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
058700091007      *
058800091007     C                   CALL      'QLGSRTIO'
058900091007     C                   PARM                    QLGSCB00
059000091007     C                   PARM                    SflRcd
059100091007     C                   PARM                    NotUsed
059200091007     C                   PARM                    SizeList
059300091007     C                   PARM                    NotUsed
059400091007     C                   PARM                    QUSEC
059500091007
059600091007     C                   ENDDO
059700091007
059800091007      * Next step - Signal end of input, clear subfile for reload.
059900091007     C                   EVAL      QLGRT00 = EndPut
060000091007     C                   CLEAR                   QUSEI
060100091007     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
060200091007
060300091007     C                   CALL      'QLGSRTIO'
060400091007     C                   PARM                    QLGSCB00
060500091007     C                   PARM                    SflRcd
060600091007     C                   PARM                    NotUsed
060700091007     C                   PARM                    SizeList
060800091007     C                   PARM                    NotUsed
060900091007     C                   PARM                    QUSEC
061000091007      *
061100091007      * ricostruisco il SFL ordinato:
061200091007      *
  pulizia SFL    ?
061300091007     C                   SETOFF                                       202128
061400091007     C                   WRITE     LR63C03
061500091007     C                   SETON                                        2021
061600091007      *
061700091007     c                   z-add     0             nrr3
061800091007      *
061900091007      * Final step - Write the records back to the subfile.
062000091007     C                   EVAL      QLGRT00 = Get
062100091007
062200091007     C                   DO        RrnLast       S3nrr_xx
062300091007     C                   CLEAR                   QUSEI
062400091007     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
062500091007     C                   CALL      'QLGSRTIO'
062600091007     C                   PARM                    QLGSCB00
062700091007     C                   PARM                    NotUsed
062800091007     C                   PARM                    SflRcd
062900091007     C                   PARM                    QLGRL00
063000091007     C                   PARM                    NotUsed
063100091007     C                   PARM                    QUSEC
063200091007      *
063300091007      * se il record non è stato ripulito totalmente
063400091007     c                   if        V3Clnp  > 0 and V3Cnsp  > 0
063500091007     C                   SETOFF                                       404150
063600091007     C                   SETOFF                                       0322
063700091007      * SFLnxtCHG
063800091007     c                   if        s03in22 = '1'
063900091007     c                   seton                                        22
064000091007     c                   end
064100091007      *
064200091007     c                   if        s03in03 = '1'
064300091007     c                   seton                                        03
064400091007     c                   end
064500091007      *
064600091007     c                   if        s03in40 = '1'
064700091007     c                   seton                                        40
064800091007     c                   end
064900091007      *
065000091007     c                   if        s03in41 = '1'
065100091007     c                   seton                                        41
065200091007     c                   end
065300091007      *
065400091007     c                   if        s03in50 = '1'
065500091007     c                   seton                                        50
065600091007     c                   end
065700091007      *
065800091007     c                   add       1             nrr3
065900091007     C                   WRITE     LR63S03
066000091007      *
066100091007     c                   endIF
066200091007      *
066300091007     C                   ENDDO
066400091007      *
066500091007      *
066600091007     C                   ENDSR
066700091007      *---------------------------------------------------------------*
066800091007      *?  Quindi accoda le Bolle con più segnalazioni                 ?
066900091007      *---------------------------------------------------------------*
067000091007     C     Accoda_Doppi  begSR
067100091007      *
067200091007      * dal SFL fittizio di salvataggio prende le righe parcheggiate.
067300091007      * e le accoda a quelle che ha precedentemente riordinato.
067400091007      *
067500091007     c                   do        sav_nrx3      NrX3
067600091007      *
067700091007      * quelli flaggati li salva
067800091007     c     nrX3          chain     LR63S03x
067900091007     c                   eval      v3CNR5   =  x3CNR5
068000091007     c                   eval      v3CBLO   =  x3CBLO
068100091007     c                   eval      v3CFTX   =  x3CFTX
068200091007     c                   eval      v3CFL1   =  x3CFL1
068300091007     c                   eval      v3CIN3   =  x3CIN3
068400091007     c                   eval      v3CDSE   =  x3CDSE
068500091007     c                   eval      s03IN03  =  x03IN03
068600091007     c                   eval      s03IN22  =  x03IN22
068700091007     c                   eval      s03IN40  =  x03IN40
068800091007     c                   eval      s03IN41  =  x03IN41
068900091007     c                   eval      s03IN50  =  x03IN50
069000091007     c                   eval      v3CSCE   =  x3CSCE
069100091007     c                   eval      v3CAAS   =  x3CAAS
069200091007     c                   eval      v3CLNP   =  x3CLNP
069300091007     c                   eval      v3CNRS   =  x3CNRS
069400091007     c                   eval      v3CNSP   =  x3CNSP
069500091007     c                   eval      v3CRMN   =  x3CRMN
069600091007     c                   eval      v3CERR   =  x3CERR
069700091007     c                   eval      v3CTPD   =  x3CTPD
069800091007     c                   eval      v3CDCM   =  x3CDCM
069900091007     c                   eval      v3CDRC  =  x3CDRC
070000091007     c                   eval      v3CDLA  =  x3CDLA
070100091007     c                   eval      v3CGIA  =  x3CGIA
070200091007     c                   eval      v3CCCA  =  x3CCCA
070300091007     c                   eval      v3CTC1  =  x3CTC1
070400091007     c                   eval      v3CSTS  =  x3CSTS
070500091007     c                   eval      v3CCOD  =  x3CCOD
070600091007     c                   eval      v3DCOD  =  x3DCOD
070700091007      * SFLnxtCHG
070800091007     C                   SetOFF                                       22  03
070900091007     C                   SetOFF                                       404150
071000091007      ***
071100091007     c                   if        s03in22 = '1'
071200091007     c                   seton                                        22
071300091007     c                   end
071400091007      *
071500091007     c                   if        s03in03 = '1'
071600091007     c                   seton                                        03
071700091007     c                   end
071800091007      *
071900091007     c                   if        s03in40 = '1'
072000091007     c                   seton                                        40
072100091007     c                   end
072200091007      *
072300091007     c                   if        s03in41 = '1'
072400091007     c                   seton                                        41
072500091007     c                   end
072600091007      *
072700091007     c                   if        s03in50 = '1'
072800091007     c                   seton                                        50
072900091007     c                   end
073000091007      *
073100091007     C                   ADD       1             NRR3
073200091007     C                   WRITE     LR63S03
073300091007      *
073400091007     c                   enddo
073500091007      *
073600091007      *  All'uscita di queste chiamate a routine di sistema il SFL record
073700091007      *   si incrementa stranamente quindi lo reimposto correttamente
073800091007      *    prima di lasciare la routine.
073900091007     C                   EVAL       NRR3 = RrnLast
074000091007     C*
074100091007      *
074200091007     C                   ENDSR
074300980401      *---------------------------------------------------------------*
074400980401      * Aggiorno FNCDE se immesso in distinta con dettaglio segnacolli
074500980401      *---------------------------------------------------------------*
074600980401     C     AFNCDE        BEGSR
074700950315      *
074800980401     C     KCDE          CHAIN     FNCDE02L                           31
074900980401      *
075000980401     C     *IN31         IFEQ      '0'
075100980401     C     KARB          CHAIN     FNCDS000                           32
075200980401      *
075300980401     C     *IN32         DOWEQ     '0'
075400980401     C     CDSNDC        IFEQ      CDENDC
075500980401     C                   EXSR      MUOVI
075600980401     C                   UPDATE    FNCDE000
075700980401     C                   SETON                                        32
075800980403     C                   EXCEPT    NOAGG
075900980401     C                   END
076000980401     C  N32KARB          READE     FNCDS000                               32
076100980401     C                   END
076200980401     C                   END
076300980401      *
076400980401     C                   ENDSR
076500980401      *---------------------------------------------------------------*
076600980401      * Muovo da FNCDS ad FNCDE per aggiornamento
076700980401      *---------------------------------------------------------------*
076800980401     C     MUOVI         BEGSR
076900980401      *
077000980401     C                   Z-ADD     CDSDCM        CDEDCM
077100980401     C                   Z-ADD     CDSHMC        CDEHMC
077200980401     C                   Z-ADD     CDSDLA        CDEDLA
077300980401     C                   Z-ADD     CDSDAG        CDEDAG
077400980401     C                   Z-ADD     CDSDRC        CDEDRC
077500980401     C                   MOVEL     CDSNO1        CDENO1
077600980401     C                   MOVEL     CDSNO2        CDENO2
077700980401     C                   MOVEL     CDSCMC        CDECMC
077800980401     C                   MOVEL     CDSCCA        CDECCA
077900000210     C                   MOVEL     CDSTC1        CDETC1
078000980401     C                   MOVEL     CDSERR        CDEERR
078100980401     C                   MOVEL     CDSSTS        CDESTS
078200980401     C                   MOVEL     CDSCOD        CDECOD
078300980401     C                   MOVEL     CDSMOT        CDEMOT
078400980401     C                   MOVEL     CDSFL1        CDEFL1
078500980401     C                   MOVEL     CDSFL2        CDEFL2
078600980401     C                   MOVEL     '1'           CDEFL3
078700980401      *
078800980401     C                   ENDSR
078900980401      *---------------------------------------------------------------*
079000980401      * Gestione videata subfile errori                               *
079100980401      *---------------------------------------------------------------*
079200980401     C     GESS03        BEGSR
079300980401      *
079400961120      * Controllo se devo inviare messaggio che non ci sono bolle
079500961120      * buone in distinta
079600980325      *
079700950315     C                   SETON                                        2021
079800970304     C     WNOREC        IFEQ      'S'
079900000418     C                   eval      *in20 = *off
080000970207     C                   END
080100000418      *
080200000418     C                   if        TSTfrz = *zeros  and  D63nrr = *zeros
080300000418     C                   EVAL      NRR3 = 1
080400000418     c                   endif
080500000418      *
080600000418     C                   SELECT
080700000418     C                   WHEN      *in28 = *on
080800000418     C                   WHEN      Wnr3key > *zeros
080900000418     C                   eval        NR3KEY = wNR3KEY
081000000418     C                   OTHER
081100000418     C                   eval        NR3KEY = 1
081200000418     c                   ENDSL
081300980325      *
081400950315     C                   WRITE     LR63Z03
081500950315     C                   EXFMT     LR63C03
081600980325      *
081700950320     C                   Z-ADD     1             NR3KEY
081800000418     C                   eval      *in28 = *off
081900000418     C                   clear                   WNR3KEY
082000980325      *  Fine Lavoro
082100950315     C     *INKC         IFEQ      '1'
082200950315     C                   MOVEL     'S'           WFINE
082300950315     C                   MOVEL     '1'           D63F03
082400950315     C                   GOTO      FINVD3
082500950315     C                   END
082600980325      *  Escludi tutte
082700961127     C   KA              EXSR      TUTTE
082800980325      *  Loop di controlli
082900950315     C                   EXSR      CTR03
083000980325      *  Se ci sono ancora dei record ricarico il subfile
083100950606     C  N28
083200950606     CAN KF              EXSR      INZS03
083300980325      *
083400950317     C     FINVD3        ENDSR
083500091005      *---------------------------------------------------------------*
083600961127      *  Escludi tutte                                                *
083700961127      *---------------------------------------------------------------*
083800961127     C     TUTTE         BEGSR
083900961127      *
084000961127     C                   MOVEL     'N'           WINTER            1
084100961127     C                   Z-ADD     1             NRR3
084200961127     C     NRR3          CHAIN     LR63S03                            32
084300980325      *
084400961127     C     *IN32         DOWEQ     '0'
084500961127     C                   SETOFF                                       414050
084600980325      *
084700980325      *  Se deve essere effettuata una scelta G/C/A/E visualizzo il campo
084800961127     C     V3CERR        IFEQ      ERR(2)
084900961127     C     V3CFTX        OREQ      'S'
085000961127     C                   SETON                                        50
085100961127     C                   END
085200980325      *  Se record con più date e lo escludo è obbligatoria una scelta
085300961127     C     V3CERR        IFEQ      ERR(2)
085400961127     C     V3CTPD        ANDNE     ' '
085500961127     C                   SETON                                        4028
085600961127     C                   Z-ADD     NRR3          NR3KEY
085700961127     C                   MOVEL     ERR(1)        $MSG
085800961127     C                   END
085900980325      *
086000980325      *  Aggiorno subfile forzando 'E' nella scelta
086100961127     C                   SETON                                        22
086200961127     C                   MOVEL     V3CIN3        *IN03
086300961127     C                   MOVEL     'E'           V3CSCE
086400961127     C                   UPDATE    LR63S03
086500961127     C                   SETOFF                                       22
086600980325      *
086700980325      *  Aggiorno subfile nascosto
086800961127     C     V3CNR5        CHAIN     LR63S05                            31
086900961127     C                   MOVEL     V3CSCE        V5CSCE
087000961127     C                   MOVEL     V3CTPD        V5CTPD
087100961127     C  N31              UPDATE    LR63S05
087200980325      *
087300980325      * Leggo RCD successivo
087400961127     C                   ADD       1             NRR3
087500961127     C     NRR3          CHAIN     LR63S03                            32
087600961127     C                   END
087700980325      *
087800961127     C                   COMMIT
087900980325      *
088000961127     C                   ENDSR
088100950315      *---------------------------------------------------------------*
088200950315      *  Loop di errori  subfile errori                               *
088300950315      *---------------------------------------------------------------*
088400950315     C     CTR03         BEGSR
088500950315      *
088600950317     C                   MOVEL     'N'           WINTER            1
088700950317     C                   READC     LR63S03                                32
088800980325      *
088900950317     C     *IN32         DOWEQ     '0'
089000950320     C     *IN28         ANDEQ     '0'
089100980325      *
089200961121     C                   SETOFF                                       414050
089300950606     C     V3CERR        IFEQ      ERR(2)
089400961120     C     V3CFTX        OREQ      'S'
089500950606     C                   SETON                                        50
089600950606     C                   END
089700980325      *  Controllo se devo richiamare la visualizzazione delle note EDI
089800961120     C     V3CTPD        IFEQ      'E'
089900961120     C     V3CFTX        IFEQ      'S'
090000961120     C                   Z-ADD     V3CAAS        D63AAS
090100961120     C                   Z-ADD     V3CLNP        D63LNP
090200961120     C                   Z-ADD     V3CNSP        D63NSP
090300961120     C                   Z-ADD     V3CNRS        D63NRS
090400000418     C                   MOVEL     fnlr63ds      KPJBU
090500961120     C                   CALL      'FNLR63R4'
090600961120     C                   PARM                    KPJBA
090700961120     C                   MOVEL     *BLANKS       V3CTPD
090800000418     C                   Z-ADD     NRR3          wNR3KEY
090900980325      *
091000961120     C                   ELSE
091100961120     C                   SETON                                        4128
091200961120     C                   Z-ADD     NRR3          NR3KEY
091300961120     C                   MOVEL     ERR(4)        $MSG
091400961120     C                   END
091500961120     C                   END
091600980325      *
091700980325      *  Se non ho + date posso scrivere solo 'E'
091800961121     C     V3CTPD        IFNE      'E'
091900961121     C     V3CTPD        ANDNE     ' '
092000961121     C     V3CERR        ANDNE     ERR(2)
092100961121     C                   SETON                                        4128
092200961121     C                   Z-ADD     NRR3          NR3KEY
092300961121     C                   MOVEL     ERR(5)        $MSG
092400961121     C                   END
092500980325      *
092600980325      *  Se forzo spedizione con + date devo prima sceglierne una data
092700961212     C     V3CTPD        IFEQ      'F'
092800961212     C     V3CTPD        ANDEQ     ' '
092900961212     C     V3CERR        ANDNE     ERR(2)
093000961212     C                   SETON                                        4128
093100961212     C                   Z-ADD     NRR3          NR3KEY
093200961212     C                   MOVEL     ERR(6)        $MSG
093300961212     C                   END
093400980325      *
093500980325      *  Interrogazione bolle
093600950317     C     V3CSCE        IFEQ      'V'
093700950317     C                   MOVEL     'S'           WINTER
093800950317     C                   MOVEL     '1'           P36FLG
093900950317     C                   Z-ADD     V3CAAS        P36AAS
094000950317     C                   Z-ADD     V3CLNP        P36LNP
094100950317     C                   Z-ADD     V3CNSP        P36NSP
094200950317     C                   Z-ADD     V3CNRS        P36NRS
094300950317     C                   MOVEL     DSLR36        KPJBU
094400950317     C                   CALL      'FNLR36R'
094500950317     C                   PARM                    KPJBA
094600950606     C                   MOVEL     ' '           V3CSCE
094700000418     C                   Z-ADD     NRR3          wNR3KEY
094800950317     C                   ELSE
094900980325      *
095000980325      *  Controllo se l'errore è forzabile oppure no !
095100961120     C     V3CBLO        IFEQ      'S'
095200961120     C     V3CSCE        ANDEQ     'F'
095300961120     C                   SETON                                        4028
095400961120     C                   Z-ADD     NRR3          NR3KEY
095500961120     C                   MOVEL     ERR(3)        $MSG
095600961120     C                   END
095700980325      *
095800980325      *  Se record con dettaglio segnacolli da analizzare non posso forzare
095900980325     C     V3CDSE        IFGE      2
096000980325     C     V3CSCE        ANDEQ     'F'
096100980325     C                   SETON                                        4028
096200980325     C                   Z-ADD     NRR3          NR3KEY
096300980325     C                   MOVEL     ERR(3)        $MSG
096400980325     C                   END
096500980325      *
096600980325      *  Se record con + date se lo escludo o lo forzo è obbligatorio scegliere
096700950606     C     V3CERR        IFEQ      ERR(2)
096800950606     C     V3CSCE        IFNE      'E'
096900961120     C     V3CSCE        ANDNE     'F'
097000961120     C     V3CTPD        ANDNE     ' '
097100950606     C                   SETON                                        4028
097200950606     C                   Z-ADD     NRR3          NR3KEY
097300950606     C                   MOVEL     ERR(1)        $MSG
097400950606     C                   END
097500950606     C                   ELSE
097600980325      *
097700950317     C*  Escludi record
097800961120     C***        V3CSCE    IFEQ 'E'
097900961120     C***        D63NRR    ANDEQ0
098000961120     C***                  SETON                     4028
098100961120     C***                  Z-ADDNRR3      NR3KEY
098200961120     C***                  MOVELERR,1     $MSG
098300961120     C***                  END
098400980325      *
098500980325      *  Gestione dettaglio segnacolli solo se esiste
098600980325     C     V3CSCE        IFEQ      'S'
098700980325     C     V3CDSE        ANDLT     2
098800980325     C                   SETON                                        4028
098900980325     C                   Z-ADD     NRR3          NR3KEY
099000980325     C                   MOVEL     ERR(7)        $MSG
099100980325     C                   END
099200980325      *
099300950606     C                   END
099400980325      *
099500950320     C                   END
099600980325      *
099700980325      *  Aggiorno subfile
099800950320     C                   SETON                                        22
099900980325     C                   MOVEL     V3CIN3        *IN03
100000950320     C                   UPDATE    LR63S03
100100950320     C                   SETOFF                                       22
100200980325      *
100300980325      *  Aggiorno subfile nascosto
100400950317     C     V3CNR5        CHAIN     LR63S05                            31
100500961122     C                   MOVEL     V3CSCE        V5CSCE
100600950606     C                   MOVEL     V3CTPD        V5CTPD
100700950317     C  N31              UPDATE    LR63S05
100800980325      *
100900950320     C  N28              READC     LR63S03                                32
101000950317     C                   END
101100980325      *
101200950317     C                   COMMIT
101300980325     C*  Se ho richiesto interogazione bolle senza selezionarne alcuna
101400950317     C*  richiamo pgm.
101500950317     C     *INKG         IFEQ      '1'
101600950317     C     WINTER        ANDEQ     'N'
101700950317     C                   MOVEL     ' '           P36FLG
101800950317     C                   Z-ADD     V3CAAS        P36AAS
101900950317     C                   Z-ADD     V3CLNP        P36LNP
102000950317     C                   Z-ADD     V3CNSP        P36NSP
102100950317     C                   Z-ADD     V3CNRS        P36NRS
102200950317     C                   MOVEL     DSLR36        KPJBU
102300950317     C                   CALL      'FNLR36R'
102400950317     C                   PARM                    KPJBA
102500950317     C                   END
102600950317     C*
102700950317     C                   ENDSR
102800950317     C*-----------------------------------------------------*
102900950317     C     *INZSR        BEGSR
103000980325      *
103100980325      * Apertura controllata dei files
103200950317     C                   OPEN      FNCDE02L                             16
103300961120     C                   OPEN      FNARB01L                             16
103400980331     C                   OPEN      FNCDS03L                             16
103500950317     C   16              GOTO      FINE                                         OPEN vado a fine
103600980325      *
103700980325      *  Definizione chiavi
103800950317     C     KCDE          KLIST
103900950317     C                   KFLD                    KNDC
104000950317     C                   KFLD                    KAAS
104100950317     C                   KFLD                    KLNP
104200950317     C                   KFLD                    KNRS
104300950317     C                   KFLD                    KNSP
104400961120     C     KARB          KLIST
104500961120     C                   KFLD                    KAAS
104600961120     C                   KFLD                    KLNP
104700961120     C                   KFLD                    KNRS
104800961120     C                   KFLD                    KNSP
104900980325      *
105000980325      *  Definizione delle variabili
105100950317     C     *LIKE         DEFINE    CDEAAS        KAAS
105200950317     C     *LIKE         DEFINE    CDELNP        KLNP
105300950317     C     *LIKE         DEFINE    CDENRS        KNRS
105400950317     C     *LIKE         DEFINE    CDENSP        KNSP
105500950317     C     *LIKE         DEFINE    CDENDC        KNDC
105600980325     C     *LIKE         DEFINE    CDENDC        §NDC
105700980325      *
105800980325      *  Inizializzazione variabili
105900950317     C                   MOVEL     'N'           WFINE             1
106000950317     C                   MOVEL     '3'           WTPVID            1
106100980325      *
106200950317     C                   ENDSR
106300980325      *----------------------------------------------------*
106400950317     OFNCDE000  E            AGGCDE
106500950317     O                       CDENDC
106600950606     OFNCDE000  E            AGGGIA
106700950606     O                       CDEDCM
106800961121     O                       CDEDLA
106900950606     OFNCDE000  E            AGGCON
107000950606     O                       CDEDAG
107100961121     O                       CDEDLA
107200961121     O                       CDECMC
107300961121     OFNCDE000  E            AGGAVV
107400961121     O                       CDEDAG
107500961121     O                       CDEDCM
107600961121     O                       CDECMC
107700961120     OFNARB000  E            AGGARB
107800020902     O                       ARBifp
107900020902     O                       ARBNDC
108000980325     OFNCDS000  E            AGGCDS
108100980325     O                       CDSNDC
108200980403     OFNCDS000  E            NOAGG
108300980325     C*----------------------------------------------------*
108400980325**
108500950320Opzione non valida
108600961121Immetti G=sped.giac./C=Conse./A=L.Avv.
108700961120Non è possibile forzare l'immissione della spedizione in distinta
108800961120Non ci sono note EDI da visualizzare
108900961121E' possibile visualizzare solo le note EDI
109000961212Obbligatorio immettere C/G/A
109100980325Non esiste dettaglio segnacolli da visualizzare
