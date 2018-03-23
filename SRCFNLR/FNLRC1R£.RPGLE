000100950127     H DECEDIT('0,') DATEDIT(*DMY.)
000200970925     H* FNLRC1R *----------------------------------------------------*
000300970925     H*         - SCRITTURA FILE DI TRASMISSIONE STOP PADRONCINI
000400950127     H*--------------------------------------------------------------*
000500970930     FFNSTPR1L  UF   E           K DISK    USROPN
000600970930     FFNSTP10L  UF   E           K DISK
000700970925     FFNSTP00T  O  A E           K DISK    USROPN
000800060215     FFiar401L  UF A E           K DISK
000900040309     Ffnblp01l  IF   E           K DISK
001000001207     FAZORG01L  IF   E           K DISK
001100970925     F*
001200020730     D C01             S              1    DIM(48) CTDATA PERRCD(48)
001300020730     D C02             S              1    DIM(48) CTDATA PERRCD(48)
001400020730     D C03             S              1    DIM(48) CTDATA PERRCD(48)
001500020730
001600020730     d mbr             s              6  0
001700020730
001800970925     D KPJBA         E DS
001900970930     D                 DS
002000970930     D  STRPDR                 1      7  0
002100970930     D  STRNDC                 8     14  0
002200970930     D  STRDDC                15     22  0
002300970930     D  STRKEY                 1     22  0
002400970930     D                 DS
002500970930     D  SAVPDR                 1      7  0
002600970930     D  SAVNDC                 8     14  0
002700970930     D  SAVDDC                15     22  0
002800970930     D  SAVKEY                 1     22  0
002900001205     D                 DS
003000001205     D  EMME                   1      1
003100020730     d  $simfel                2      4  0
003200020730     d  caetfe                 5      7  0
003300001205     D  VAR22                  1     10
003400040309     D DSBL4A        E DS
003500970925     D CNCR80        E DS
003600970925     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003700001205     D DSCMZ         E DS
003800020730     d tibs55ds      e ds
003900060216     d Dataiso         s               d   datfmt(*iso)
004000970925     C*****************************************************************
004100970925     C*  RIEPILOGO INDICATORI
004200970925     C*****************************************************************
004300970930     C*
004400001205     C* IMPOSTO ALCUNI VALORI FISSI X CREARE FILE TRASMISSIONE
004500001205     C                   EXSR      IMPO
004600060216      *
004700970925     C* ALLOCAZIONE MEMBRO DI TRASMISSIONE A SEDE
004800051028     C                   MOVEL     'FNSTP00T'    SFILE             8
004900970925     C                   MOVEL     'FNSTP00T'    FISICO
005000040309     c                   movel     946           mbr
005100020730     c                   move      046           mbr
005200970925     C                   EXSR      ALCOBJ
005300970925     C*
005400970925     C* PROSEGUO SOLO SE SONO RIUSCITA AD ALLOCARE IL MEMBRO X LA SEDE
005500970925     C     *IN91         CABEQ     *ON           FINE
005600970930     C* OVRDBF MEMBRO PER LA SEDE
005700040309     c                   movel     946           mbr
005800020730     c                   move      046           mbr
005900970930     C                   MOVEL     'FNSTP00T'    SFILE
006000970930     C                   EXSR      OVERRF
006100970930     C                   OPEN      FNSTP00T
006200970930     C****
006300970930     C* E L A B O R A Z I O N E   F N S T P R 0 F
006400970930     C* (STOP DI RITIRO DEI PADRONCINI DIPENDENTI)
006500001207     C                   EXSR      ELATPR
006600001207     C                   CLOSE     FNSTPR1L
006700971106     C****
006800971106     C* E L A B O R A Z I O N E   F N S T P 1 0 F
006900051026     C* (CONSEGNE E RESTANTI RITIRI) sia x sede che filiale
007000001207     C                   EXSR      ELTP1S
007100001205      * -----
007200001207     C*  INVIA MEMBRO SEDE
007300010312     C                   FEOD      FNSTP00T
007400040309     C                   CLOSE     FNSTP00T
007500040309     c
007600001207     C                   MOVE      046           CAETFE
007700040309     c                   movel     946           $simfel
007800020731     c                   movel     var22         cmzmba
007900020731     C                   MOVEL     VAR22         CMZMBD
008000001207     C                   EXSR      CALTBS
008100001207     C                   MOVE      '046'         CMZDST
008200001205     C                   EXSR      LCKSTP
008300040309     c
008400051026     c                   movel     simfel        mdlc              6
008500971106     C****
008600040309     c                   movel     946           mdlc
008700040309     C                   MOVE      '046'         MDLC
008800040309     C                   EXSR      DLCOBJ
008900970923     C*
009000040309     C     FINE          TAG
009100950127     C                   SETON                                        LR
009200001207     C****************************************************************
009300001207     C* E L A B O R A Z I O N E   F N S T P R 0 F
009400001207     C****************************************************************
009500001207     C     ELATPR        BEGSR
009600001207      *
009700001207     C                   CLEAR                   COMSET
009800001207     C                   CLEAR                   SAVKEY
009900001207     C                   CLEAR                   SAVSTP
010000001207     C                   CLEAR                   SAVKSC
010100001207     C                   CLEAR                   SAVRSC
010200001207     C                   OPEN      FNSTPR1L
010300001207     C     *LOVAL        SETLL     FNSTPR1L
010400001207     C                   READ      FNSTPR1L                               30
010500001207    1C     *IN30         DOWEQ     *OFF
010600001207     C* OPERAZIONI A ROTTURA
010700001207    2C                   SELECT
010800001207     C* ROTTURA DI CLIENTE/RAGIONE SOCIALE
010900001207     C* (VALE SOLO SE NUMER STOP = 0 ALTRIMENTI DEVO CONTEGGIARE LE
011000001207     C* SPEDIZIONI PER NUMERO STOP)
011100001207     C     STRKSC        WHENNE    SAVKSC
011200001207     C     SAVSTP        ANDEQ     0
011300001207     C     STRRSC        ORNE      SAVRSC
011400001207     C     SAVSTP        ANDEQ     0
011500001207    3C     COMSET        IFGT      *ZEROS
011600001207     C                   CLEAR                   WCMG
011700001207     C                   EXSR      ELASTP
011800001207    3C                   END
011900001207     C                   EXSR      AZZROT
012000001207     C* ROTTURA DI NUMERO STOP
012100001207     C     STRSTP        WHENNE    SAVSTP
012200001207     C     SAVSTP        IFNE      99999
012300001207     C                   CLEAR                   WCMG
012400001207     C                   EXSR      ELASTP
012500001207     C                   END
012600001207     C                   EXSR      AZZROT
012700001207     C* ROTTURA DI PADRONCINO/DISTINTA
012800001207     C     STRKEY        WHENNE    SAVKEY
012900001207     C     SAVSTP        IFNE      99999
013000001207     C                   CLEAR                   WCMG
013100001207     C                   EXSR      ELASTP
013200001207     C                   END
013300001207     C                   EXSR      AZZROT
013400001207    2C                   ENDSL
013500001207     C* CALCOLO IL NUMERO DI SPEDIZIONI PER STOP
013600001207     C     STRSTP        IFNE      99999
013700001207     C                   ADD       1             COMSET
013800001207     C                   ELSE
013900001207     C                   Z-ADD     STRSTP        COMSET
014000001207     C                   MOVEL     'S'           WCMG              1
014100001207     C                   EXSR      ELASTP
014200001207     C                   CLEAR                   COMSET
014300001207     C                   END
014400001207     C                   READ      FNSTPR1L                               30
014500001207    1C                   ENDDO
014600001207     C* ESEGUO OPERAZIONI ROTTURA DI FINE CICLO
014700001207    1C     COMSET        IFGT      *ZEROS
014800001207     C                   EXSR      ELASTP
014900001207    1C                   END
015000001207     C*
015100001207     C                   ENDSR
015200001207     C****************************************************************
015300051026     C* E L A B O R A Z I O N E   F N S T P 1 0 F  X SEDE  e filiale
015400001207     C****************************************************************
015500001207     C     ELTP1S        BEGSR
015600001207     C*
015700001207     C     *LOVAL        SETLL     FNSTP10L
015800001207     C                   READ      FNSTP10L                               30
015900001207    1C     *IN30         DOWEQ     *OFF
016000001207     C*
016100001207    2C     STPINV        IFEQ      'E'
016200001207     C     STPINV        OREQ      'S'
016300001207     C                   WRITE     FNSTP000
016400001207    2C                   END
016500001207     C* CANCELLO RECORD DA FILE DI APPOGGIO SE ERA SOLO PER SEDE
016600001207     C* ALTRIMENTI AGGIORNO STPINV='F' PER TRASMETTERLO ALLA FILIALE
016700001207    2C     STPINV        IFEQ      'S'
016800001207     C                   DELETE    FNSTP100
016900001207     C                   ELSE
017000051026     c* Per filiale aggiorno subito la bolla perchè non invia + niente
017100051026     c                   exsr      AGGIOBOLLA
017200060228     C  n99              DELETE    FNSTP100
017300001207    2C                   ENDIF
017400001207     C*
017500001207     C                   READ      FNSTP10L                               30
017600001207    1C                   ENDDO
017700001207     C*
017800001207     C                   ENDSR
017900051026     C****************************************************************
018000051026     c* Aggiorno subito la bolla di filiale
018100051026     C****************************************************************
018200051026     c     AGGIOBOLLA    BEGSR
018300051026     c* solo se c'e' la bolla
018400060510     c     kblp          chain     fnblp01l                           39
018500060510   4ac                   if        not *in39
018600051026     C                   CLEAR                   DSBL4A
018700060215     C     KBL4          CHAIN     Fiar401L                           30
018800060510     c* Se record in ar4 non trovato, imposto anche il flag stampa DDT
018900060510     c*  e la sigla operatore
019000060510     c                   if        *in30
019100060510     C                   clear                   ar4not
019200060510     c                   movel     blpfst        §b4abm
019300060510     c                   movel     blpsop        §b4aso
019400060510     c                   else
019500060510     C                   MOVEL     ar4NOT        DSBL4A
019600060510     c                   endif
019700060216     c
019800051026    5C     STPTSR        IFEQ      'R'
019900051026     C                   Z-ADD     STPSET        §B4ASR
020000051026   X5C                   ELSE
020100051026     C                   Z-ADD     STPSET        §B4ASC
020200051026    5C                   END
020300060215     C                   MOVEL     DSBL4A        ar4NOT
020400060216     C*
020500060216     C                   TIME                    W0140            14 0
020600060216     C                   MOVE      W0140         WDTGIO            8 0
020700060216     c     *eur          move      wdtgio        dataiso
020800060216     c                   movel     dataiso       ar4duv
020900051026    5C     *IN30         IFEQ      *ON
021000060215     C                   Z-ADD     STPAAS        ar4AAS
021100060215     C                   Z-ADD     STPLNP        ar4LNP
021200060215     C                   Z-ADD     STPNRS        ar4NRS
021300060215     C                   Z-ADD     STPNSP        ar4NSP
021400060215     C                   MOVEL     WTRC          ar4TRC
021500060216     c                   movel     'T'           ar4ftr
021600060216     c                   movel     dataiso       ar4dtr
021700060216     c                   endif
021800060216     c
021900060228     c                   setoff                                       99
022000060228     C   30              WRITE     Fiar4000                             99
022100060216     C  n30              UPDATE    Fiar4000
022200051026   4aC                   ENDIF
022300051026     c                   ENDSR
022400970930     C****************************************************************
022500970930     C* SCRITTURA RECORDS STOP DI RITIRO PADRONCINI DIPENDENTI
022600970930     C****************************************************************
022700970930     C     ELASTP        BEGSR
022800970930     C* RILETTURA FNSTPR1L PER AGGIORNAMENTO BL4 E TRASMIS. STOP A SEDE
022900970930     C* SE SAVSTP = 0 LEGGO PER CLIENTE
023000970930     C* ALTRIMENTI LEGGO PER NUMERO STOP
023100970930    1C     SAVSTP        IFEQ      *ZEROS
023200970930     C     KSTR          SETLL     FNSTPR1L
023300970930     C     KSTR          READE     FNSTPR1L                               30
023400970930   X1C                   ELSE
023500000615     C     SAVSTP        IFNE      99999
023600970930     C     KSTRS         SETLL     FNSTPR1L
023700970930     C     KSTRS         READE     FNSTPR1L                               30
023800970930     C                   END
023900970930    1C                   END
024000060216    c
024100970930    1C     *IN30         DOWEQ     *OFF
024200060216     c
024300060216     C* AGGIORNO Fiar4 TIPO RECORD 'A' solo se c'e' la bolla in blp
024400060216    2C     STRINV        IFEQ      'F'
024500970930     C     STRINV        OREQ      'E'
024600060216     c
024700060510     c     kblpr         chain     fnblp01l                           39
024800060510    3c                   if        not *in39
024900970930     C                   CLEAR                   DSBL4A
025000060216     C     KBL4R         CHAIN     Fiar401L                           30
025100060510     c*
025200060510     c* Se record in ar4 non trovato, imposto anche il flag stampa DDT
025300060510     c*  e la sigla operatore
025400060510     c                   if        *in30
025500060510     C                   clear                   ar4not
025600060510     c                   movel     blpfst        §b4abm
025700060510     c                   movel     blpsop        §b4aso
025800060510     c                   else
025900060510     C                   MOVEL     ar4NOT        DSBL4A
026000060510     c                   endif
026100060510     c
026200970930     C                   Z-ADD     COMSET        §B4ASR
026300060216     C                   MOVEL     DSBL4A        ar4NOT
026400060216     C*
026500060216     C                   TIME                    W0140            14 0
026600060216     C                   MOVE      W0140         WDTGIO            8 0
026700060216     c     *eur          move      wdtgio        dataiso
026800060216     c                   movel     dataiso       ar4duv
026900060216     c
027000060216    4C     *IN30         IFEQ      *ON
027100060216     C                   Z-ADD     STRAAS        ar4AAS
027200060216     C                   Z-ADD     STRLNP        ar4LNP
027300060216     C                   Z-ADD     STRNRS        ar4NRS
027400060216     C                   Z-ADD     STRNSP        ar4NSP
027500060216     C                   MOVEL     WTRC          ar4TRC
027600060216     c                   movel     'T'           ar4ftr
027700060216     c                   movel     dataiso       ar4dtr
027800060216    4c                   endif
027900060216     c
028000060228     c                   setoff                                       99
028100060228     C   30              WRITE     Fiar4000                             99
028200060216     C  n30              UPDATE    Fiar4000
028300060216    3C                   END
028400060216    2C                   END
028500060216     c
028600970930     C* SCRIVO RECORD DI TRASMISSIONE PER SEDE
028700060228    2C  n99STRINV        IFEQ      'S'
028800970930     C     STRINV        OREQ      'E'
028900970930     C                   MOVEL     'R'           STPTSR
029000970930     C                   Z-ADD     STRAAS        STPAAS
029100970930     C                   Z-ADD     STRLNP        STPLNP
029200970930     C                   Z-ADD     STRNRS        STPNRS
029300970930     C                   Z-ADD     STRNSP        STPNSP
029400000615     C*SE NUMERO STOP <>99999(NO CONS.A MAG.) E NUMERO SPED./STOP=99999
029500000615     C*LO CAMBIO METTENDO 99998 PER NON CONFONDERLO CON LE CONS.A MAG.
029600060228    3C     WCMG          IFEQ      *BLANKS
029700000615     C     COMSET        ANDEQ     99999
029800000615     C                   Z-ADD     99998         STPSET
029900971027     C                   ELSE
030000970930     C                   Z-ADD     COMSET        STPSET
030100060228    3C                   END
030200970930     C                   WRITE     FNSTP000
030300060228    2C                   END
030400060228     c* Non deleto se non riuscita write (per sicurezza, visto che è
030500060228     c*  accaduto)
030600060228     C  n99              DELETE    FNSTPR00
030700060228     c
030800970930    2C     SAVSTP        IFEQ      *ZEROS
030900970930     C     KSTR          READE     FNSTPR1L                               30
031000970930   X2C                   ELSE
031100000615     C     SAVSTP        IFNE      99999
031200970930     C     KSTRS         READE     FNSTPR1L                               30
031300970930     C                   ELSE
031400970930     C                   SETON                                        30
031500970930     C                   END
031600970930    2C                   END
031700970930    1C                   ENDDO
031800970930     C* LEGGO ANCORA FNSTPR1L PER RIPOSIZIONARMI SULL'ULTIMO RECOR LETT
031900000615     C     SAVSTP        IFNE      99999
032000971001     C     KSTR          SETGT     FNSTPR1L
032100970930     C                   READ      FNSTPR1L                               30
032200970930     C                   END
032300970930     C                   ENDSR
032400970930     C****************************************************************
032500970930     C* AZZERAMENTI A ROTTURA
032600970930     C****************************************************************
032700970930     C     AZZROT        BEGSR
032800970930     C                   CLEAR                   COMSET
032900970930     C  N30              MOVE      STRKEY        SAVKEY
033000970930     C  N30              MOVE      STRSTP        SAVSTP
033100970930     C  N30              MOVE      STRKSC        SAVKSC
033200970930     C  N30              MOVE      STRRSC        SAVRSC
033300970930     C                   ENDSR
033400970925     C**************************************************************************
033500970925     C*    ALLOCO OGGETTI
033600970925     C**************************************************************************
033700970925     C     ALCOBJ        BEGSR
033800970925     C*
033900020730     C                   MOVE      '))'          VAR2              8
034000020730     C                   Z-ADD     48            LUNG             15 5
034100970925     C*
034200970925     C                   MOVE      MBR           MBRM
034300970925     C                   MOVEA     SFILE         C02(13)
034400020730     C                   MOVEA     C02           SALC             48
034500970925     C* CHECK OBJ E ADDPFM
034600970925     C                   MOVEL     MBRM          FISMBR
034700970925     C                   CALL      'TRUL50C'
034800970925     C                   PARM                    FISICO           10
034900970925     C                   PARM                    FISMBR           10
035000971114     C                   PARM                    KNSIF
035100970925     C                   PARM                    LOGICO           10
035200970925     C                   PARM                    LOGMBR           10
035300970925     C                   PARM                    FLG               1
035400970925     C*
035500970925     C* ALLOCO OGGETTO
035600970925     C                   MOVEL     MBR           VAR2
035700970925     C                   MOVE      VAR2          SALC
035800970925     C                   MOVEL     *BLANKS       COMMAN           80
035900970925     C                   MOVEL     SALC          COMMAN
036000970925     C                   CALL      'QCMDEXC'                            91
036100970925     C                   PARM                    COMMAN
036200970925     C                   PARM                    LUNG
036300970925     C*
036400970925     C                   ENDSR
036500970925     C**************************************************************************
036600970925     C*  OVRDBF SU MEMBRO
036700060216     C**************************************************************************
036800970925     C     OVERRF        BEGSR
036900970925     C*
037000020730     C                   MOVE      ')'           VAR               7
037100970925     C                   MOVEA     SFILE         C01(13)
037200020730     C                   MOVEA     C01           SOVR             48
037300970925     C** OVRDBF
037400970925     C                   MOVEL     MBR           VAR
037500970925     C                   MOVE      VAR           SOVR
037600970925     C                   MOVEL     *BLANKS       COMMAN
037700970925     C                   MOVEL     SOVR          COMMAN
037800970925     C                   CALL      'QCMDEXC'
037900970925     C                   PARM                    COMMAN
038000970925     C                   PARM                    LUNG
038100970925     C*
038200970925     C                   ENDSR
038300970925     C**************************************************************************
038400970925     C*    DISALLOCO OGGETTI
038500970925     C**************************************************************************
038600970925     C     DLCOBJ        BEGSR
038700970925     C*
038800020730     C                   MOVE      '))'          VAR2
038900020730     C                   Z-ADD     48            LUNG             15 5
039000970925     C*
039100970925     C                   MOVEA     SFILE         C03(13)
039200020730     C                   MOVEA     C03           SDLC             48
039300970925     C*
039400970925     C                   MOVEL     MDLC          VAR2
039500970925     C                   MOVE      VAR2          SDLC
039600970925     C                   MOVEL     *BLANKS       COMMAN           80
039700970925     C                   MOVEL     SDLC          COMMAN
039800970925     C                   CALL      'QCMDEXC'                            91
039900970925     C                   PARM                    COMMAN
040000970925     C                   PARM                    LUNG
040100970925     C                   ENDSR
040200001205     C**************************************************************************
040300001205     C** IMPOSTO ALCUNI DATI FISSI PER CREARE I FILE DI TRASMISSIONE
040400001205     C**************************************************************************
040500001205     C     IMPO          BEGSR
040600001205     C*
040700001205     C                   MOVEL     'M'           EMME
040800001205     C                   MOVE      '0000'        CMZORI                          ORA
040900001205     C                   MOVE      '0000000'     CMZDTI                          DATA
041000001205     C                   MOVE      'R'           CMZURG                          N=IMMED
041100001205     C                   MOVE      'A'           CMZADD                          IN ADD
041200001205     C                   MOVE      'S'           CMZCLR                          CLEAR
041300001205     C                   MOVE      'S'           CMZFTR                          DA TRANS.
041400001205     C                   MOVE      *BLANK        CMZERR                          1= ERR.
041500001205     C                   ENDSR
041600001207     C**************************************************************************
041700001207     C*  CALL AL PROGRAMMA CHE REPERISCE LIBRERIA E S.I DI TRAMISSIONE
041800001207     C**************************************************************************
041900001207     C     CALTBS        BEGSR
042000001207     C*
042100001207     C     CAETFE        CHAIN     AZORG01L                           31
042200001207     C   31              MOVEL     CAETFE        ORGFEL
042300001207     C*
042400020730     C                   CLEAR                   tibs55ds
042500001207     C                   MOVEL     KNSIF         I50PSI
042600001207     C                   MOVEL     SIMFEL        I50PPO
042700001207     C                   MOVEL     CAETFE        I50APO
042800020730     C                   CALL      'TIBS55R'
042900020730     C                   PARM                    tibs55ds
043000001207     C**
043100001207     C                   MOVEL     O50PLA        SAVLBD
043200001207     C                   MOVEL     O50ALA        SAVLBA
043300001207     C                   MOVEL     O50ASI        CMZSYF
043400001207     C                   MOVEL     O50PLG        WLBLBD
043500001207     C                   MOVEL     O50ALG        WLBLBA
043600001207     C                   ENDSR
043700001205     C**************************************************************************
043800001207     C*  INVIA MEMBRI FILE STOP PADRONCINI
043900001205     C**************************************************************************
044000001205     C     LCKSTP        BEGSR
044100001205     C*
044200001205     C* VEDO SE CONTIENE RECORD
044300001205     C                   MOVEL     'FNSTP00T'    CMZFLD
044400001205     C                   EXSR      NUMREC
044500001205     C                   Z-ADD     NRR           RECSTP           10 0
044600001205     C* INVIO
044700001205     C                   MOVEL     'TR45'        CMZAZN
044800001205     C                   MOVEL     *BLANKS       CMZPRR
044900001205     C                   MOVEL     'FNSTP00R'    CMZFLA
045000001205     C                   MOVE      RECSTP        CMZNRR
045100001205     C                   MOVEL     SAVLBD        CMZLBD
045200001205     C                   MOVEL     SAVLBA        CMZLBA
045300001205     C*
045400001205     C                   EXSR      CALCMZ
045500001205     C*
045600001205     C                   ENDSR
045700001205     C**************************************************************************
045800001205     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
045900001205     C**************************************************************************
046000001205     C     NUMREC        BEGSR
046100001205     C*
046200001205     C                   Z-ADD     0             NRR
046300001205     C                   CALL      'TRUL51C'
046400001205     C                   PARM                    KNSIF
046500001205     C                   PARM                    CMZFLD           10
046600001205     C                   PARM                    VAR22            10
046700001205     C                   PARM                    NRR              10 0
046800001205     C*
046900001205     C                   ENDSR
047000001205     C**************************************************************************
047100001205     C*  CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
047200001205     C**************************************************************************
047300001205     C     CALCMZ        BEGSR
047400001205     C*
047500001205     C                   CLEAR                   W236
047600001205     C* IMPOSTO SE I 2 P.O. SONO LOCALI
047700001205     C                   MOVEL     O50LCL        CMZLCL            1
047800020730     C                   CALL      'TIBS53C'
047900001205     C                   PARM                    CMZLCL            1
048000001205     C                   PARM                    CMZDST
048100001205     C                   PARM                    CMZFLD
048200001205     C                   PARM                    CMZMBD
048300001205     C                   PARM                    CMZLBD
048400001205     C                   PARM                    CMZFLA
048500001205     C                   PARM                    CMZMBA
048600001205     C                   PARM                    CMZLBA
048700001205     C                   PARM                    CMZORI
048800001205     C                   PARM                    CMZDTI
048900001205     C                   PARM                    CMZURG
049000001205     C                   PARM                    CMZADD
049100001205     C                   PARM                    CMZCLR
049200001205     C                   PARM                    CMZFTR
049300001205     C                   PARM                    CMZERR
049400001205     C                   PARM                    CMZAZN
049500001205     C                   PARM                    CMZSYF
049600001205     C                   PARM                    CMZNRR
049700001205     C                   PARM                    CMZPRR
049800001205     C                   PARM                    W236            236
049900001205     C*
050000001205     C                   ENDSR
050100001207     C****************************************************************
050200001207     C     *INZSR        BEGSR
050300001207     C****************************************************************
050400001207     C*
050500001207     C     *ENTRY        PLIST
050600001207     C                   PARM                    KPJBA
050700001207     C*
050800001207     C                   Z-ADD     1             CODUT
050900001207     C                   CALL      'X§PARUT'
051000001207     C                   PARM                    UT§DSE
051100001207     C                   MOVEL     RAGUT         RSUT             20
051200001207     C                   MOVEL     REC80         CNCR80
051300001207     C*
051400020730     C                   MOVEL     'M'           MBRM              7
051500001207     C*
051600001207     C     *LIKE         DEFINE    STPLNP        WMBR
051700060216     C     *LIKE         DEFINE    ar4TRC        WTRC
051800001207     C     *LIKE         DEFINE    STPSET        COMSET
051900001207     C     *LIKE         DEFINE    STRKSC        SAVKSC
052000001207     C     *LIKE         DEFINE    STRRSC        SAVRSC
052100001207     C     *LIKE         DEFINE    STRSTP        SAVSTP
052200001207      * -----
052300001207     C     *LIKE         DEFINE    CMZLBD        SAVLBD
052400001207     C     *LIKE         DEFINE    CMZLBA        SAVLBA
052500001207     C     *LIKE         DEFINE    CMZLBD        WLBLBD
052600001207     C     *LIKE         DEFINE    CMZLBA        WLBLBA
052700001207      * -----
052800001207     C*
052900001207     C     KBL4          KLIST
053000001207     C                   KFLD                    STPAAS
053100001207     C                   KFLD                    STPLNP
053200001207     C                   KFLD                    STPNRS
053300001207     C                   KFLD                    STPNSP
053400001207     C                   KFLD                    WTRC
053500040309     C     KBLP          KLIST
053600040309     C                   KFLD                    STPAAS
053700040309     C                   KFLD                    STPLNP
053800040309     C                   KFLD                    STPNRS
053900040309     C                   KFLD                    STPNSP
054000001207     C*
054100001207     C     KBL4R         KLIST
054200001207     C                   KFLD                    STRAAS
054300001207     C                   KFLD                    STRLNP
054400001207     C                   KFLD                    STRNRS
054500001207     C                   KFLD                    STRNSP
054600001207     C                   KFLD                    WTRC
054700060216     C     KBLPR         KLIST
054800060216     C                   KFLD                    STRAAS
054900060216     C                   KFLD                    STRLNP
055000060216     C                   KFLD                    STRNRS
055100060216     C                   KFLD                    STRNSP
055200001207     C*
055300001207     C     KSTR          KLIST
055400001207     C                   KFLD                    SAVPDR
055500001207     C                   KFLD                    SAVNDC
055600001207     C                   KFLD                    SAVDDC
055700001207     C                   KFLD                    SAVSTP
055800001207     C                   KFLD                    SAVKSC
055900001207     C                   KFLD                    SAVRSC
056000001207     C*
056100001207     C     KSTRS         KLIST
056200001207     C                   KFLD                    SAVPDR
056300001207     C                   KFLD                    SAVNDC
056400001207     C                   KFLD                    SAVDDC
056500001207     C                   KFLD                    SAVSTP
056600001207     C*
056700001207     C                   MOVEL     'A'           WTRC
056800001207     C*
056900001207     C                   ENDSR
057000970925**         C01
057100020730OVRDBF FILE(FNSTP00T)               MBR(M000000)
057200970925**         C02
057300020730ALCOBJ OBJ((FNSTP00T   *FILE   *EXCL   M000000))
057400970925**         C03
057500020730DLCOBJ OBJ((FNSTP00T   *FILE   *EXCL   M000000))
