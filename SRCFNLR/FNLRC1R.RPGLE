000100950127     H DECEDIT('0,') DATEDIT(*DMY.)
000200970925     H* FNLRC1R *----------------------------------------------------*
000300970925     H*         - SCRITTURA FILE DI TRASMISSIONE STOP PADRONCINI
000400950127     H*--------------------------------------------------------------*
000500970930     FFNSTPR1L  UF   E           K DISK    USROPN
000600970930     FFNSTP10L  UF   E           K DISK
000700970925     FFNSTP00T  O  A E           K DISK    USROPN
000800060215     FFiar401L  UF A E           K DISK
000900040309     Ffnblp01l  IF   E           K DISK
001000070928     F***AZORG01L  IF   E           K DISK
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
017100070928     c* 28/09/07 ES --> non aggiortno più la bolla in quanto i campi degli
017200070928     c*             stops non servono più
017300070928     c                   if        *in99 and not *in99
017400051026     c                   exsr      AGGIOBOLLA
017500070928     c                   endif
017600070928     c
017700060228     C  n99              DELETE    FNSTP100
017800001207    2C                   ENDIF
017900001207     C*
018000001207     C                   READ      FNSTP10L                               30
018100001207    1C                   ENDDO
018200001207     C*
018300001207     C                   ENDSR
018400051026     C****************************************************************
018500051026     c* Aggiorno subito la bolla di filiale
018600051026     C****************************************************************
018700051026     c     AGGIOBOLLA    BEGSR
018800051026     c* solo se c'e' la bolla
018900060510     c     kblp          chain     fnblp01l                           39
019000060510   4ac                   if        not *in39
019100051026     C                   CLEAR                   DSBL4A
019200060215     C     KBL4          CHAIN     Fiar401L                           30
019300060510     c* Se record in ar4 non trovato, imposto anche il flag stampa DDT
019400060510     c*  e la sigla operatore
019500060510     c                   if        *in30
019600060510     C                   clear                   ar4not
019700060510     c                   movel     blpfst        §b4abm
019800060510     c                   movel     blpsop        §b4aso
019900060510     c                   else
020000060510     C                   MOVEL     ar4NOT        DSBL4A
020100060510     c                   endif
020200060216     c
020300051026    5C     STPTSR        IFEQ      'R'
020400051026     C                   Z-ADD     STPSET        §B4ASR
020500051026   X5C                   ELSE
020600051026     C                   Z-ADD     STPSET        §B4ASC
020700051026    5C                   END
020800060215     C                   MOVEL     DSBL4A        ar4NOT
020900060216     C*
021000060216     C                   TIME                    W0140            14 0
021100060216     C                   MOVE      W0140         WDTGIO            8 0
021200060216     c     *eur          move      wdtgio        dataiso
021300060216     c                   movel     dataiso       ar4duv
021400051026    5C     *IN30         IFEQ      *ON
021500060215     C                   Z-ADD     STPAAS        ar4AAS
021600060215     C                   Z-ADD     STPLNP        ar4LNP
021700060215     C                   Z-ADD     STPNRS        ar4NRS
021800060215     C                   Z-ADD     STPNSP        ar4NSP
021900060215     C                   MOVEL     WTRC          ar4TRC
022000060216     c                   movel     'T'           ar4ftr
022100060216     c                   movel     dataiso       ar4dtr
022200060216     c                   endif
022300060216     c
022400060228     c                   setoff                                       99
022500060228     C   30              WRITE     Fiar4000                             99
022600060216     C  n30              UPDATE    Fiar4000
022700051026   4aC                   ENDIF
022800051026     c                   ENDSR
022900970930     C****************************************************************
023000970930     C* SCRITTURA RECORDS STOP DI RITIRO PADRONCINI DIPENDENTI
023100970930     C****************************************************************
023200970930     C     ELASTP        BEGSR
023300970930     C* RILETTURA FNSTPR1L PER AGGIORNAMENTO BL4 E TRASMIS. STOP A SEDE
023400970930     C* SE SAVSTP = 0 LEGGO PER CLIENTE
023500970930     C* ALTRIMENTI LEGGO PER NUMERO STOP
023600970930    1C     SAVSTP        IFEQ      *ZEROS
023700970930     C     KSTR          SETLL     FNSTPR1L
023800970930     C     KSTR          READE     FNSTPR1L                               30
023900970930   X1C                   ELSE
024000000615     C     SAVSTP        IFNE      99999
024100970930     C     KSTRS         SETLL     FNSTPR1L
024200970930     C     KSTRS         READE     FNSTPR1L                               30
024300970930     C                   END
024400970930    1C                   END
024500060216    c
024600970930    1C     *IN30         DOWEQ     *OFF
024700060216     c
024800060216     C* AGGIORNO Fiar4 TIPO RECORD 'A' solo se c'e' la bolla in blp
024900070928    2C***  STRINV        IFEQ      'F'
025000070928     C***  STRINV        OREQ      'E'
025100070928     c***
025200070928     c***  kblpr         chain     fnblp01l                           39
025300070928    3c***                if        not *in39
025400070928     C***                CLEAR                   DSBL4A
025500070928     C***  KBL4R         CHAIN     Fiar401L                           30
025600070928     c***
025700070928     c**Se record in ar4 non trovato, imposto anche il flag stampa DDT
025800070928     c***e la sigla operatore
025900070928     c***                if        *in30
026000070928     C***                clear                   ar4not
026100070928     c***                movel     blpfst        §b4abm
026200070928     c***                movel     blpsop        §b4aso
026300070928     c***                else
026400070928     C***                MOVEL     ar4NOT        DSBL4A
026500070928     c***                endif
026600070928     c***
026700070928     C***                Z-ADD     COMSET        §B4ASR
026800070928     C***                MOVEL     DSBL4A        ar4NOT
026900070928     C***
027000070928     C***                TIME                    W0140            14 0
027100070928     C***                MOVE      W0140         WDTGIO            8 0
027200070928     c***  *eur          move      wdtgio        dataiso
027300070928     c***                movel     dataiso       ar4duv
027400070928     c***
027500070928    4C***  *IN30         IFEQ      *ON
027600070928     C***                Z-ADD     STRAAS        ar4AAS
027700070928     C***                Z-ADD     STRLNP        ar4LNP
027800070928     C***                Z-ADD     STRNRS        ar4NRS
027900070928     C***                Z-ADD     STRNSP        ar4NSP
028000070928     C***                MOVEL     WTRC          ar4TRC
028100070928     c***                movel     'T'           ar4ftr
028200070928     c***                movel     dataiso       ar4dtr
028300070928    4c***                endif
028400070928     c***
028500070928     c***                setoff                                       99
028600070928     C***30              WRITE     Fiar4000                             99
028700070928     C**n30              UPDATE    Fiar4000
028800070928    3C***                END
028900070928    2C***                END
029000060216     c
029100970930     C* SCRIVO RECORD DI TRASMISSIONE PER SEDE
029200060228    2C  n99STRINV        IFEQ      'S'
029300970930     C     STRINV        OREQ      'E'
029400970930     C                   MOVEL     'R'           STPTSR
029500970930     C                   Z-ADD     STRAAS        STPAAS
029600970930     C                   Z-ADD     STRLNP        STPLNP
029700970930     C                   Z-ADD     STRNRS        STPNRS
029800970930     C                   Z-ADD     STRNSP        STPNSP
029900000615     C*SE NUMERO STOP <>99999(NO CONS.A MAG.) E NUMERO SPED./STOP=99999
030000000615     C*LO CAMBIO METTENDO 99998 PER NON CONFONDERLO CON LE CONS.A MAG.
030100060228    3C     WCMG          IFEQ      *BLANKS
030200000615     C     COMSET        ANDEQ     99999
030300000615     C                   Z-ADD     99998         STPSET
030400971027     C                   ELSE
030500970930     C                   Z-ADD     COMSET        STPSET
030600060228    3C                   END
030700970930     C                   WRITE     FNSTP000
030800060228    2C                   END
030900060228     c* Non deleto se non riuscita write (per sicurezza, visto che è
031000060228     c*  accaduto)
031100060228     C  n99              DELETE    FNSTPR00
031200060228     c
031300970930    2C     SAVSTP        IFEQ      *ZEROS
031400970930     C     KSTR          READE     FNSTPR1L                               30
031500970930   X2C                   ELSE
031600000615     C     SAVSTP        IFNE      99999
031700970930     C     KSTRS         READE     FNSTPR1L                               30
031800970930     C                   ELSE
031900970930     C                   SETON                                        30
032000970930     C                   END
032100970930    2C                   END
032200970930    1C                   ENDDO
032300970930     C* LEGGO ANCORA FNSTPR1L PER RIPOSIZIONARMI SULL'ULTIMO RECOR LETT
032400000615     C     SAVSTP        IFNE      99999
032500971001     C     KSTR          SETGT     FNSTPR1L
032600970930     C                   READ      FNSTPR1L                               30
032700970930     C                   END
032800970930     C                   ENDSR
032900970930     C****************************************************************
033000970930     C* AZZERAMENTI A ROTTURA
033100970930     C****************************************************************
033200970930     C     AZZROT        BEGSR
033300970930     C                   CLEAR                   COMSET
033400970930     C  N30              MOVE      STRKEY        SAVKEY
033500970930     C  N30              MOVE      STRSTP        SAVSTP
033600970930     C  N30              MOVE      STRKSC        SAVKSC
033700970930     C  N30              MOVE      STRRSC        SAVRSC
033800970930     C                   ENDSR
033900970925     C**************************************************************************
034000970925     C*    ALLOCO OGGETTI
034100970925     C**************************************************************************
034200970925     C     ALCOBJ        BEGSR
034300970925     C*
034400020730     C                   MOVE      '))'          VAR2              8
034500020730     C                   Z-ADD     48            LUNG             15 5
034600970925     C*
034700970925     C                   MOVE      MBR           MBRM
034800970925     C                   MOVEA     SFILE         C02(13)
034900020730     C                   MOVEA     C02           SALC             48
035000970925     C* CHECK OBJ E ADDPFM
035100970925     C                   MOVEL     MBRM          FISMBR
035200970925     C                   CALL      'TRUL50C'
035300970925     C                   PARM                    FISICO           10
035400970925     C                   PARM                    FISMBR           10
035500971114     C                   PARM                    KNSIF
035600970925     C                   PARM                    LOGICO           10
035700970925     C                   PARM                    LOGMBR           10
035800970925     C                   PARM                    FLG               1
035900970925     C*
036000970925     C* ALLOCO OGGETTO
036100970925     C                   MOVEL     MBR           VAR2
036200970925     C                   MOVE      VAR2          SALC
036300970925     C                   MOVEL     *BLANKS       COMMAN           80
036400970925     C                   MOVEL     SALC          COMMAN
036500970925     C                   CALL      'QCMDEXC'                            91
036600970925     C                   PARM                    COMMAN
036700970925     C                   PARM                    LUNG
036800970925     C*
036900970925     C                   ENDSR
037000970925     C**************************************************************************
037100970925     C*  OVRDBF SU MEMBRO
037200060216     C**************************************************************************
037300970925     C     OVERRF        BEGSR
037400970925     C*
037500020730     C                   MOVE      ')'           VAR               7
037600970925     C                   MOVEA     SFILE         C01(13)
037700020730     C                   MOVEA     C01           SOVR             48
037800970925     C** OVRDBF
037900970925     C                   MOVEL     MBR           VAR
038000970925     C                   MOVE      VAR           SOVR
038100970925     C                   MOVEL     *BLANKS       COMMAN
038200970925     C                   MOVEL     SOVR          COMMAN
038300970925     C                   CALL      'QCMDEXC'
038400970925     C                   PARM                    COMMAN
038500970925     C                   PARM                    LUNG
038600970925     C*
038700970925     C                   ENDSR
038800970925     C**************************************************************************
038900970925     C*    DISALLOCO OGGETTI
039000970925     C**************************************************************************
039100970925     C     DLCOBJ        BEGSR
039200970925     C*
039300020730     C                   MOVE      '))'          VAR2
039400020730     C                   Z-ADD     48            LUNG             15 5
039500970925     C*
039600970925     C                   MOVEA     SFILE         C03(13)
039700020730     C                   MOVEA     C03           SDLC             48
039800970925     C*
039900970925     C                   MOVEL     MDLC          VAR2
040000970925     C                   MOVE      VAR2          SDLC
040100970925     C                   MOVEL     *BLANKS       COMMAN           80
040200970925     C                   MOVEL     SDLC          COMMAN
040300970925     C                   CALL      'QCMDEXC'                            91
040400970925     C                   PARM                    COMMAN
040500970925     C                   PARM                    LUNG
040600970925     C                   ENDSR
040700001205     C**************************************************************************
040800001205     C** IMPOSTO ALCUNI DATI FISSI PER CREARE I FILE DI TRASMISSIONE
040900001205     C**************************************************************************
041000001205     C     IMPO          BEGSR
041100001205     C*
041200001205     C                   MOVEL     'M'           EMME
041300001205     C                   MOVE      '0000'        CMZORI                          ORA
041400001205     C                   MOVE      '0000000'     CMZDTI                          DATA
041500001205     C                   MOVE      'R'           CMZURG                          N=IMMED
041600001205     C                   MOVE      'A'           CMZADD                          IN ADD
041700001205     C                   MOVE      'S'           CMZCLR                          CLEAR
041800001205     C                   MOVE      'S'           CMZFTR                          DA TRANS.
041900001205     C                   MOVE      *BLANK        CMZERR                          1= ERR.
042000001205     C                   ENDSR
042100001207     C**************************************************************************
042200001207     C*  CALL AL PROGRAMMA CHE REPERISCE LIBRERIA E S.I DI TRAMISSIONE
042300001207     C**************************************************************************
042400001207     C     CALTBS        BEGSR
042500001207     C*
042600070928     C***  CAETFE        CHAIN     AZORG01L                           31
042700070928     C***31              MOVEL     CAETFE        ORGFEL
042800001207     C*
042900020730     C                   CLEAR                   tibs55ds
043000001207     C                   MOVEL     KNSIF         I50PSI
043100001207     C                   MOVEL     SIMFEL        I50PPO
043200001207     C                   MOVEL     CAETFE        I50APO
043300020730     C                   CALL      'TIBS55R'
043400020730     C                   PARM                    tibs55ds
043500001207     C**
043600001207     C                   MOVEL     O50PLA        SAVLBD
043700001207     C                   MOVEL     O50ALA        SAVLBA
043800001207     C                   MOVEL     O50ASI        CMZSYF
043900001207     C                   MOVEL     O50PLG        WLBLBD
044000001207     C                   MOVEL     O50ALG        WLBLBA
044100001207     C                   ENDSR
044200001205     C**************************************************************************
044300001207     C*  INVIA MEMBRI FILE STOP PADRONCINI
044400001205     C**************************************************************************
044500001205     C     LCKSTP        BEGSR
044600001205     C*
044700001205     C* VEDO SE CONTIENE RECORD
044800001205     C                   MOVEL     'FNSTP00T'    CMZFLD
044900001205     C                   EXSR      NUMREC
045000001205     C                   Z-ADD     NRR           RECSTP           10 0
045100001205     C* INVIO
045200001205     C                   MOVEL     'TR45'        CMZAZN
045300001205     C                   MOVEL     *BLANKS       CMZPRR
045400001205     C                   MOVEL     'FNSTP00R'    CMZFLA
045500001205     C                   MOVE      RECSTP        CMZNRR
045600001205     C                   MOVEL     SAVLBD        CMZLBD
045700001205     C                   MOVEL     SAVLBA        CMZLBA
045800001205     C*
045900001205     C                   EXSR      CALCMZ
046000001205     C*
046100001205     C                   ENDSR
046200001205     C**************************************************************************
046300001205     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE
046400001205     C**************************************************************************
046500001205     C     NUMREC        BEGSR
046600001205     C*
046700001205     C                   Z-ADD     0             NRR
046800001205     C                   CALL      'TRUL51C'
046900001205     C                   PARM                    KNSIF
047000001205     C                   PARM                    CMZFLD           10
047100001205     C                   PARM                    VAR22            10
047200001205     C                   PARM                    NRR              10 0
047300001205     C*
047400001205     C                   ENDSR
047500001205     C**************************************************************************
047600001205     C*  CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
047700001205     C**************************************************************************
047800001205     C     CALCMZ        BEGSR
047900001205     C*
048000001205     C                   CLEAR                   W236
048100001205     C* IMPOSTO SE I 2 P.O. SONO LOCALI
048200001205     C                   MOVEL     O50LCL        CMZLCL            1
048300020730     C                   CALL      'TIBS53C'
048400001205     C                   PARM                    CMZLCL            1
048500001205     C                   PARM                    CMZDST
048600001205     C                   PARM                    CMZFLD
048700001205     C                   PARM                    CMZMBD
048800001205     C                   PARM                    CMZLBD
048900001205     C                   PARM                    CMZFLA
049000001205     C                   PARM                    CMZMBA
049100001205     C                   PARM                    CMZLBA
049200001205     C                   PARM                    CMZORI
049300001205     C                   PARM                    CMZDTI
049400001205     C                   PARM                    CMZURG
049500001205     C                   PARM                    CMZADD
049600001205     C                   PARM                    CMZCLR
049700001205     C                   PARM                    CMZFTR
049800001205     C                   PARM                    CMZERR
049900001205     C                   PARM                    CMZAZN
050000001205     C                   PARM                    CMZSYF
050100001205     C                   PARM                    CMZNRR
050200001205     C                   PARM                    CMZPRR
050300001205     C                   PARM                    W236            236
050400001205     C*
050500001205     C                   ENDSR
050600001207     C****************************************************************
050700001207     C     *INZSR        BEGSR
050800001207     C****************************************************************
050900001207     C*
051000001207     C     *ENTRY        PLIST
051100001207     C                   PARM                    KPJBA
051200001207     C*
051300001207     C                   Z-ADD     1             CODUT
051400001207     C                   CALL      'X§PARUT'
051500001207     C                   PARM                    UT§DSE
051600001207     C                   MOVEL     RAGUT         RSUT             20
051700001207     C                   MOVEL     REC80         CNCR80
051800001207     C*
051900020730     C                   MOVEL     'M'           MBRM              7
052000001207     C*
052100001207     C     *LIKE         DEFINE    STPLNP        WMBR
052200060216     C     *LIKE         DEFINE    ar4TRC        WTRC
052300001207     C     *LIKE         DEFINE    STPSET        COMSET
052400001207     C     *LIKE         DEFINE    STRKSC        SAVKSC
052500001207     C     *LIKE         DEFINE    STRRSC        SAVRSC
052600001207     C     *LIKE         DEFINE    STRSTP        SAVSTP
052700001207      * -----
052800001207     C     *LIKE         DEFINE    CMZLBD        SAVLBD
052900001207     C     *LIKE         DEFINE    CMZLBA        SAVLBA
053000001207     C     *LIKE         DEFINE    CMZLBD        WLBLBD
053100001207     C     *LIKE         DEFINE    CMZLBA        WLBLBA
053200001207      * -----
053300001207     C*
053400001207     C     KBL4          KLIST
053500001207     C                   KFLD                    STPAAS
053600001207     C                   KFLD                    STPLNP
053700001207     C                   KFLD                    STPNRS
053800001207     C                   KFLD                    STPNSP
053900001207     C                   KFLD                    WTRC
054000040309     C     KBLP          KLIST
054100040309     C                   KFLD                    STPAAS
054200040309     C                   KFLD                    STPLNP
054300040309     C                   KFLD                    STPNRS
054400040309     C                   KFLD                    STPNSP
054500001207     C*
054600001207     C     KBL4R         KLIST
054700001207     C                   KFLD                    STRAAS
054800001207     C                   KFLD                    STRLNP
054900001207     C                   KFLD                    STRNRS
055000001207     C                   KFLD                    STRNSP
055100001207     C                   KFLD                    WTRC
055200060216     C     KBLPR         KLIST
055300060216     C                   KFLD                    STRAAS
055400060216     C                   KFLD                    STRLNP
055500060216     C                   KFLD                    STRNRS
055600060216     C                   KFLD                    STRNSP
055700001207     C*
055800001207     C     KSTR          KLIST
055900001207     C                   KFLD                    SAVPDR
056000001207     C                   KFLD                    SAVNDC
056100001207     C                   KFLD                    SAVDDC
056200001207     C                   KFLD                    SAVSTP
056300001207     C                   KFLD                    SAVKSC
056400001207     C                   KFLD                    SAVRSC
056500001207     C*
056600001207     C     KSTRS         KLIST
056700001207     C                   KFLD                    SAVPDR
056800001207     C                   KFLD                    SAVNDC
056900001207     C                   KFLD                    SAVDDC
057000001207     C                   KFLD                    SAVSTP
057100001207     C*
057200001207     C                   MOVEL     'A'           WTRC
057300001207     C*
057400001207     C                   ENDSR
057500970925**         C01
057600020730OVRDBF FILE(FNSTP00T)               MBR(M000000)
057700970925**         C02
057800020730ALCOBJ OBJ((FNSTP00T   *FILE   *EXCL   M000000))
057900970925**         C03
058000020730DLCOBJ OBJ((FNSTP00T   *FILE   *EXCL   M000000))
