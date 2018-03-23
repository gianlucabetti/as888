000100961007     H DECEDIT('0,') DATEDIT(*DMY.)
000200961007     H*---------------------------------------------------------------*
000300961007     H*  Ritorno date arrivo ai partner  - E.D.I. - Estero -          *
000400961007     F*---------------------------------------------------------------*
000500961007     F*  Effettuo la quadratura degli errori rilevati in ricezione    *
000600961007     F*  bolle con quello che ho realmente confermato tenendo presente*
000700961007     F*  che assumo come validi i dati passati nel PCI.               *
000800961007     F*  - Se ho confermato la bolla tenendo per buoni i PCI gli      *
000900961007     F*    errori possibili dovrebbero essere già stati rilevati      *
001000961007     F*    dal giro delle spunte.                                     *
001100961007     F*  - Se ho confermato la bolla in modo errato devo vedere       *
001200961007     F*    quali sono gli errori dei partner che ho già corretto      *
001300961007     F*    in fase di bollettazione + altri errori eventuali          *
001400961015     F*    --------------------------------------------------------   *
001500961015     F*  Effettuo la quadratura degli errori rilevati in ricezione    *
001600961015     F*  bolle con quelli rilevati in fase di elaborazione delle      *
001700961015     F*  spunte.                                                      *
001800961015     F*  - Se ho letto un errore di manca collo mi posiziono su       *
001900961015     F*    EDBRV x codice cliente, numero foglio entrata, lnp,        *
002000961015     F*    serie e segnacollo e controllo se ho degli errori di       *
002100961015     F*    eccedenza. In caso affermativo annullo gli errori sia      *
002200961015     F*    in EDSUM che in EDBRV.                                     *
002300961009     F*    --------------------------------------------------------   *
002400961009     F*  Considero solo i record di EDSUM con il nr.foglio entrata a  *
002500961009     F*  zero. Quelli che hanno il nr.foglio entrata già impostato    *
002600961009     F*  sono già stati selezionati da precedenti conferme di invii   *
002700961007     F*---------------------------------------------------------------*
002800961007     F*  DATA BASE                                                    *
002900961007     F*---------------------------------------------------------------*
003000961015     FEDSUM02L  UF   E           K DISK
003100961007     F*---------
003200980324     FEDSUM05L  UF   E           K DISK
003300980324     F                                     RENAME(EDSUM000:EDSUM005)
003400980324     F*---------
003500961025     FEDBRV02L  UF   E           K DISK
003600980409     F*---------
003700980409     FEDBRV03L  UF   E           K DISK
003800980409     F                                     RENAME(EDBRV000:EDBRVECC)
003900961015     F*---------
004000961007     FFNBLT01L  IF   E           K DISK
004100961007     D*---------------------------------------------------------------------*
004200961007     D* DS
004300961007     D*---------------------------------------------------------------------*
004400961007     D KPJBA         E DS
004500961007     D TRTC87        E DS                  EXTNAME(TRTC87DS)
004600961007     D DSPU          E DS                  EXTNAME(EDIDSPU)
004700961008     D*
004800961008     D*  Chiave per rottura numero spedizione
004900961008     D SAVSPE          DS
005000961008     D  WSVAAS                 1      4  0
005100961008     D  WSVLNP                 5      7  0
005200961008     D  WSVNRS                 8      9  0
005300961008     D  WSVNSP                10     16  0
005400961008     D NEWSPE          DS
005500961008     D  BLTAAS                 1      4  0
005600961008     D  BLTLNP                 5      7  0
005700961008     D  BLTNRS                 8      9  0
005800961008     D  BLTNSP                10     16  0
005900961011     D SUMSPE          DS
006000961011     D  SUMAAS                 1      4  0
006100961011     D  SUMLNP                 5      7  0
006200961011     D  SUMNRS                 8      9  0
006300961011     D  SUMNSP                10     16  0
006400961007     C*---------------------------------------------------------------------*
006500961007     C*       C I C L O         P R I N C I P A L E
006600961007     C*---------------------------------------------------------------------*
006700980324     C*  Lavoro per codice cliente
006800980324     C     D87LNP        IFEQ      0
006900961007     C*  Eseguo posizionamento iniziale in base al codice cliente
007000961007     C                   Z-ADD     D87CCM        KCCM
007100961009     C                   Z-ADD     0             KNFE
007200961011     C                   MOVEL     *BLANKS       KFLG
007300961015     C     KSUM          CHAIN     EDSUM02L                           30
007400961007     C     *IN30         DOWEQ     '0'
007500961007     C*  Mi posiziono su FNBLT per effettuare la quadratura
007600961007     C                   EXSR      CHNBLT
007700961007     C*  Eseguo quadratura BLT - EDSUM se non sono a fine file
007800961007     C     *IN30         IFEQ      '0'
007900961008     C     *IN31         ANDEQ     '0'
008000961007     C                   EXSR      GESQUA
008100961007     C                   END
008200961007     C*
008300961007     C                   END
008400980324     C*  Lavoro per lnp
008500980324     C                   ELSE
008600980324     C*  Eseguo posizionamento iniziale in base al codice cliente
008700980324     C                   Z-ADD     D87LNP        KLNP
008800980324     C                   Z-ADD     0             KNFE
008900980324     C                   MOVEL     *BLANKS       KFLG
009000980324     C     KSUM1         CHAIN     EDSUM05L                           30
009100980324     C     *IN30         DOWEQ     '0'
009200980324     C*  Mi posiziono su FNBLT per effettuare la quadratura
009300980324     C                   EXSR      CHBLT5
009400980324     C*  Eseguo quadratura BLT - EDSUM se non sono a fine file
009500980324     C     *IN30         IFEQ      '0'
009600980324     C     *IN31         ANDEQ     '0'
009700980324     C                   EXSR      GESQU5
009800980324     C                   END
009900980324     C*
010000980324     C                   END
010100980324     C*
010200980324     C                   END                                                    D87LNP=0
010300980409     C*  Se non devo tornare eccedenze annullo logicamente
010400980409     C*  il codice stato 002
010500980409     C     D87SEC        IFEQ      *BLANKS
010600980409     C                   EXSR      DLTECC
010700980409     C                   END
010800961007     C*  Fine Lavoro
010900961007     C                   SETON                                        LR
011000961007     C*-----------------------------------------------------------*
011100961007     C* CHNBLT: Eseguo posizionamento iniziale su FNBLT
011200961007     C*-----------------------------------------------------------*
011300961007     C     CHNBLT        BEGSR
011400961007     C*
011500961007     C                   SETON                                        31
011600961007     C*-----------------------------------------------------------*
011700961007     C*  Se non aggangia la bolla (provab. perchè risegnacollata) *
011800961007     C*  non aggiorno EDSUM. In questo modo darò sempre errore di *
011900961007     C*  collo eccedente. Dovrà essere poi l'utente che a video   *
012000961007     C*  provvederà eventualmente a sposare il numero dei segnac. *
012100961007     C*  ricevuti dal cliente con i nuovi segnacolli della bolla. *
012200961007     C*  In tal caso dovrà procedere all'annullamento delle relati.
012300961007     C*  anomalie.                                                *
012400961007     C*-----------------------------------------------------------*
012500961008     C     *IN31         DOWEQ     '1'                                          Loop chain BLT
012600961008     C     *IN30         ANDEQ     '0'
012700961007     C                   Z-ADD     SUMAAS        KAAS
012800961007     C                   Z-ADD     SUMLNP        KLNP
012900961007     C                   Z-ADD     SUMNRS        KNRS
013000961007     C                   Z-ADD     SUMNSP        KNSP
013100961007     C     KBLT          CHAIN     FNBLT01L                           31
013200961030     C                   MOVEL     SUMSPE        SAVSPE
013300961007     C*-----------------------------------------------------------*
013400961008     C*  Procedo quindi aggiornando tutti i record della spedizio-*
013500961008     C*  ne impostndo il codice di errore manca collo !!          *
013600961007     C*-----------------------------------------------------------*
013700961008     C     *IN31         IFEQ      '1'                                          if  sped.no BLT
013800961008     C     *IN30         DOWEQ     '0'                                          loop spedizione
013900961011     C     SAVSPE        ANDEQ     SUMSPE
014000961015     C*  Prima di dare l'errore di manca collo controllo se c'è
014100961015     C*  errore di eccedenza in EDBRV
014200961114     C                   MOVEL     'N'           WTRECC
014300961114     C***                  EXSR CHNBRV
014400961015     C     WTRECC        IFEQ      'N'
014500961008     C                   MOVEL     '001'         SUMSTS
014600961008     C                   EXCEPT    AGGERR
014700961015     C                   END
014800961015     C     KSUM          READE     EDSUM02L                               30
014900961008     C                   END                                                    loop spedizione
015000961008     C                   END                                                    end sped.no BLT
015100961007     C*
015200961008     C                   END                                                    Loop chain BLT
015300961008     C*
015400961007     C                   ENDSR
015500980324     C*-----------------------------------------------------------*
015600980324     C* CHBLT5: Eseguo posizionamento iniziale su FNBLT
015700980324     C*-----------------------------------------------------------*
015800980324     C     CHBLT5        BEGSR
015900980324     C*
016000980324     C                   SETON                                        31
016100980324     C*-----------------------------------------------------------*
016200980324     C*  Se non aggangia la bolla (provab. perchè risegnacollata) *
016300980324     C*  non aggiorno EDSUM. In questo modo darò sempre errore di *
016400980324     C*  collo eccedente. Dovrà essere poi l'utente che a video   *
016500980324     C*  provvederà eventualmente a sposare il numero dei segnac. *
016600980324     C*  ricevuti dal cliente con i nuovi segnacolli della bolla. *
016700980324     C*  In tal caso dovrà procedere all'annullamento delle relati.
016800980324     C*  anomalie.                                                *
016900980324     C*-----------------------------------------------------------*
017000980324     C     *IN31         DOWEQ     '1'                                          Loop chain BLT
017100980324     C     *IN30         ANDEQ     '0'
017200980324     C                   Z-ADD     SUMAAS        KAAS
017300980324     C                   Z-ADD     SUMLNP        KLNP
017400980324     C                   Z-ADD     SUMNRS        KNRS
017500980324     C                   Z-ADD     SUMNSP        KNSP
017600980324     C     KBLT          CHAIN     FNBLT01L                           31
017700980324     C                   MOVEL     SUMSPE        SAVSPE
017800980324     C*-----------------------------------------------------------*
017900980324     C*  Procedo quindi aggiornando tutti i record della spedizio-*
018000980324     C*  ne impostndo il codice di errore manca collo !!          *
018100980324     C*-----------------------------------------------------------*
018200980324     C     *IN31         IFEQ      '1'                                          if  sped.no BLT
018300980324     C     *IN30         DOWEQ     '0'                                          loop spedizione
018400980324     C     SAVSPE        ANDEQ     SUMSPE
018500980324     C*  Prima di dare l'errore di manca collo controllo se c'è
018600980324     C*  errore di eccedenza in EDBRV
018700980324     C                   MOVEL     'N'           WTRECC
018800980324     C     WTRECC        IFEQ      'N'
018900980324     C                   MOVEL     '001'         SUMSTS
019000980324     C                   EXCEPT    AGGER5
019100980324     C                   END
019200980324     C     KSUM1         READE     EDSUM05L                               30
019300980324     C                   END                                                    loop spedizione
019400980324     C                   END                                                    end sped.no BLT
019500980324     C*
019600980324     C                   END                                                    Loop chain BLT
019700980324     C*
019800980324     C                   ENDSR
019900961007     C*-----------------------------------------------------------*
020000961007     C* GESQUA: Eseguo quadratura
020100961007     C*-----------------------------------------------------------*
020200961007     C     GESQUA        BEGSR
020300961008     C*-------------------------------------------------------------*
020400961008     C*  Fino a che elaboro la spedizione aggiorno EDSUM fleggando  *
020500961008     C*  i record.                                                  *
020600961008     C*-------------------------------------------------------------*
020700961008     C     *IN30         DOWEQ     '0'                                          loop spedizione
020800961011     C     SAVSPE        ANDEQ     SUMSPE
020900961007     C*-------------------------------------------------------------*
021000961008     C*  ... se il numero segnacollo di EDSUM è inferiore a quello  *
021100961008     C*  della bolla devo dare errore di manca                      *
021200961007     C*-------------------------------------------------------------*
021300961011     C     BLTNSC        DOWGT     SUMNSC                                       loop nr.sgn BLT
021400961011     C     *IN30         ANDEQ     '0'
021500961011     C     SAVSPE        ANDEQ     SUMSPE
021600961015     C*  Prima di dare l'errore di manca collo controllo se c'è
021700961015     C*  errore di eccedenza in EDBRV
021800961015     C                   EXSR      CHNBRV
021900961015     C     WTRECC        IFEQ      'N'
022000961011     C                   MOVEL     '001'         SUMSTS
022100961011     C                   EXCEPT    AGGERR
022200961015     C                   END
022300961015     C     KSUM          READE     EDSUM02L                               30
022400961008     C                   END                                                    loop nr.sgn BLT
022500961011     C*
022600961008     C*-------------------------------------------------------------*
022700961008     C*  ... se il numero segnacollo di EDSUM è uguale a quello     *
022800961008     C*  della bolla annullo il record di EDSUM in modo che non     *
022900961008     C*  vengano segnalati errori.                                  *
023000961008     C*-------------------------------------------------------------*
023100961009     C     BLTNSC        DOWEQ     SUMNSC                                       loop nr.sgn BLT
023200961008     C     SAVSPE        ANDEQ     NEWSPE
023300961011     C     SAVSPE        ANDEQ     SUMSPE
023400961008     C     *IN31         ANDEQ     '0'                                          = nr.sgn.EDSUM
023500961008     C     *IN30         ANDEQ     '0'
023600961015     C*  Controllo se c'è errore di eccedenza in EDBRV
023700961015     C                   EXSR      CHNBRV
023800961015     C     WTRECC        IFEQ      'N'
023900961008     C                   MOVEL     '*'           SUMFLG
024000961031     C                   MOVEL     D87NFE        SUMNFE
024100961008     C                   EXCEPT    AGGANN
024200961015     C                   END
024300961008     C     KBLT          READE     FNBLT01L                               31
024400961015     C     KSUM          READE     EDSUM02L                               30
024500961008     C                   END                                                    loop nr.sgn BLT
024600961011     C*
024700961011     C     *IN31         IFEQ      '1'
024800961011     C     *IN30         DOWEQ     '0'
024900961011     C     BLTNSC        ANDLT     SUMNSC                                       loop nr.sgn BLT
025000961011     C     SAVSPE        ANDEQ     SUMSPE
025100961015     C*  Prima di dare l'errore di manca collo controllo se c'è
025200961015     C*  errore di eccedenza in EDBRV
025300961114     C                   EXSR      CHNBRV
025400961015     C     WTRECC        IFEQ      'N'
025500961011     C                   MOVEL     '001'         SUMSTS
025600961015     C                   EXCEPT    AGGERR
025700961015     C                   END
025800961015     C     KSUM          READE     EDSUM02L                               30
025900961011     C                   END
026000961011     C                   END
026100961008     C*-------------------------------------------------------------*
026200961008     C                   END                                                    loop spedizione
026300961008     C*-------------------------------------------------------------*
026400961008     C                   ENDSR
026500980324     C*-----------------------------------------------------------*
026600980324     C* GESQU5: Eseguo quadratura
026700980324     C*-----------------------------------------------------------*
026800980324     C     GESQU5        BEGSR
026900980324     C*-------------------------------------------------------------*
027000980324     C*  Fino a che elaboro la spedizione aggiorno EDSUM fleggando  *
027100980324     C*  i record.                                                  *
027200980324     C*-------------------------------------------------------------*
027300980324     C     *IN30         DOWEQ     '0'                                          loop spedizione
027400980324     C     SAVSPE        ANDEQ     SUMSPE
027500980324     C*-------------------------------------------------------------*
027600980324     C*  ... se il numero segnacollo di EDSUM è inferiore a quello  *
027700980324     C*  della bolla devo dare errore di manca                      *
027800980324     C*-------------------------------------------------------------*
027900980324     C     BLTNSC        DOWGT     SUMNSC                                       loop nr.sgn BLT
028000980324     C     *IN30         ANDEQ     '0'
028100980324     C     SAVSPE        ANDEQ     SUMSPE
028200980324     C*  Prima di dare l'errore di manca collo controllo se c'è
028300980324     C*  errore di eccedenza in EDBRV
028400980324     C                   EXSR      CHNBRV
028500980324     C     WTRECC        IFEQ      'N'
028600980324     C                   MOVEL     '001'         SUMSTS
028700980324     C                   EXCEPT    AGGER5
028800980324     C                   END
028900980324     C     KSUM1         READE     EDSUM05L                               30
029000980324     C                   END                                                    loop nr.sgn BLT
029100980324     C*
029200980324     C*-------------------------------------------------------------*
029300980324     C*  ... se il numero segnacollo di EDSUM è uguale a quello     *
029400980324     C*  della bolla annullo il record di EDSUM in modo che non     *
029500980324     C*  vengano segnalati errori.                                  *
029600980324     C*-------------------------------------------------------------*
029700980324     C     BLTNSC        DOWEQ     SUMNSC                                       loop nr.sgn BLT
029800980324     C     SAVSPE        ANDEQ     NEWSPE
029900980324     C     SAVSPE        ANDEQ     SUMSPE
030000980324     C     *IN31         ANDEQ     '0'                                          = nr.sgn.EDSUM
030100980324     C     *IN30         ANDEQ     '0'
030200980324     C*  Controllo se c'è errore di eccedenza in EDBRV
030300980324     C                   EXSR      CHNBRV
030400980324     C     WTRECC        IFEQ      'N'
030500980324     C                   MOVEL     '*'           SUMFLG
030600980324     C                   MOVEL     D87NFE        SUMNFE
030700980324     C                   EXCEPT    AGGAN5
030800980324     C                   END
030900980324     C     KBLT          READE     FNBLT01L                               31
031000980324     C     KSUM1         READE     EDSUM05L                               30
031100980324     C                   END                                                    loop nr.sgn BLT
031200980324     C*
031300980324     C     *IN31         IFEQ      '1'
031400980324     C     *IN30         DOWEQ     '0'
031500980324     C     BLTNSC        ANDLT     SUMNSC                                       loop nr.sgn BLT
031600980324     C     SAVSPE        ANDEQ     SUMSPE
031700980324     C*  Prima di dare l'errore di manca collo controllo se c'è
031800980324     C*  errore di eccedenza in EDBRV
031900980324     C                   EXSR      CHNBRV
032000980324     C     WTRECC        IFEQ      'N'
032100980324     C                   MOVEL     '001'         SUMSTS
032200980324     C                   EXCEPT    AGGER5
032300980324     C                   END
032400980324     C     KSUM1         READE     EDSUM05L                               30
032500980324     C                   END
032600980324     C                   END
032700980324     C*-------------------------------------------------------------*
032800980324     C                   END                                                    loop spedizione
032900980324     C*-------------------------------------------------------------*
033000980324     C                   ENDSR
033100961015     C*-----------------------------------------------------------*
033200961015     C* CHNBRV: Eseguo posizionamento iniziale su EDBRV
033300961015     C*-----------------------------------------------------------*
033400961015     C     CHNBRV        BEGSR
033500961015     C*
033600961015     C                   MOVEL     'N'           WTRECC            1
033700980327     C                   MOVEL     *BLANKS       KCAN
033800980327     C                   MOVEL     *BLANKS       KFLG
033900961015     C                   MOVEL     '002'         KSTS
034000961015     C                   Z-ADD     D87NFE        KNFE1
034100961029     C                   Z-ADD     SUMFLS        KFLS
034200961015     C                   Z-ADD     SUMNRS        KNRS
034300961015     C                   Z-ADD     SUMNSC        KNSC
034400961025     C     KBRV          CHAIN     EDBRV02L                           32
034500961015     C     *IN32         IFEQ      '0'                                          Loop chain BLT
034600961015     C                   MOVEL     'S'           WTRECC
034700961015     C                   MOVEL     SUMNSP        BRVNSP
034800961030     C                   MOVEL     SUMLNP        BRVLNP
034900961030     C                   MOVEL     SUMAAS        BRVAAS
035000961015     C                   MOVEL     '*'           BRVFLG
035100961015     C                   EXCEPT    AGGBRV
035200961015     C                   MOVEL     '*'           SUMFLG
035300961031     C                   MOVEL     D87NFE        SUMNFE
035400961015     C                   EXCEPT    AGGANN
035500961015     C                   END
035600961015     C*
035700961015     C                   ENDSR
035800980409     C*-----------------------------------------------------------*
035900980409     C* DLTECC: Annullo eccedenze 002
036000980409     C*-----------------------------------------------------------*
036100980409     C     DLTECC        BEGSR
036200980409     C*
036300980409     C                   MOVEL     'N'           WTRECC            1
036400980409     C                   MOVEL     *BLANKS       KFLG
036500980409     C                   MOVEL     '002'         KSTS
036600980409     C                   Z-ADD     D87NFE        KNFE1
036700980409     C                   Z-ADD     SUMFLS        KFLS
036800980409     C     D87LNP        IFEQ      0
036900980409     C     KBRV2         CHAIN     EDBRV02L                           32
037000980409     C                   ELSE
037100980409     C     KBRV3         CHAIN     EDBRV03L                           32
037200980409     C                   END
037300980409     C     *IN32         DOWEQ     '0'                                          Loop chain BLT
037400980409     C                   MOVEL     '*'           BRVFLG
037500980409     C     D87LNP        IFEQ      0
037600980409     C                   EXCEPT    AGGBR2
037700980409     C     KBRV2         READE     EDBRV02L                               32
037800980409     C                   ELSE
037900980409     C                   EXCEPT    AGGBR3
038000980409     C     KBRV3         READE     EDBRV03L                               32
038100980409     C                   END
038200980409     C                   END
038300980409     C*
038400980409     C                   ENDSR
038500961008     C*-----------------------------------------------------------*
038600961008     C* *INZSR: Operazioni iniziali
038700961008     C*-----------------------------------------------------------*
038800961008     C     *INZSR        BEGSR
038900961008     C*
039000961008     C     *ENTRY        PLIST
039100961008     C                   PARM                    KPJBA
039200961008     C                   MOVEL     KPJBU         TRTC87
039300961008     C*
039400961008     C* Chiavi di accesso
039500961008     C     KBLT          KLIST
039600961008     C                   KFLD                    KAAS
039700961008     C                   KFLD                    KLNP
039800961008     C                   KFLD                    KNRS
039900961008     C                   KFLD                    KNSP
040000961009     C     KSUM          KLIST
040100961009     C                   KFLD                    KCCM
040200961009     C                   KFLD                    KNFE
040300961011     C                   KFLD                    KFLG
040400980324     C     KSUM1         KLIST
040500980324     C                   KFLD                    KLNP
040600980324     C                   KFLD                    KNFE
040700980324     C                   KFLD                    KFLG
040800961015     C     KBRV          KLIST
040900961015     C                   KFLD                    KCCM
041000961015     C                   KFLD                    KNFE1
041100961015     C                   KFLD                    KFLG
041200961015     C                   KFLD                    KSTS
041300980327     C                   KFLD                    KCAN
041400961029     C                   KFLD                    KFLS
041500961015     C                   KFLD                    KNRS
041600961015     C                   KFLD                    KNSC
041700980409     C     KBRV2         KLIST
041800980409     C                   KFLD                    KCCM
041900980409     C                   KFLD                    KNFE1
042000980409     C                   KFLD                    KFLG
042100980409     C                   KFLD                    KSTS
042200980409     C     KBRV3         KLIST
042300980409     C                   KFLD                    KLNP
042400980409     C                   KFLD                    KNFE1
042500980409     C                   KFLD                    KFLG
042600980409     C                   KFLD                    KSTS
042700961008     C*
042800961008     C* Definizione chiave di accesso
042900961008     C     *LIKE         DEFINE    BLTAAS        KAAS
043000961008     C     *LIKE         DEFINE    BLTLNP        KLNP
043100961008     C     *LIKE         DEFINE    BLTNRS        KNRS
043200961008     C     *LIKE         DEFINE    BLTNSP        KNSP
043300961008     C     *LIKE         DEFINE    SUMCCM        KCCM
043400961011     C     *LIKE         DEFINE    SUMNFE        KNFE
043500961015     C     *LIKE         DEFINE    BRVNFE        KNFE1
043600961011     C     *LIKE         DEFINE    SUMFLG        KFLG
043700961029     C     *LIKE         DEFINE    BRVFLS        KFLS
043800961015     C     *LIKE         DEFINE    BRVNSC        KNSC
043900961015     C     *LIKE         DEFINE    BRVSTS        KSTS
044000980327     C     *LIKE         DEFINE    BRVCAN        KCAN
044100961008     C*
044200961008     C                   ENDSR
044300961009     O*-------------------------------------------------------------*
044400961009     OEDSUM000  E            AGGANN
044500961009     O                       SUMFLG
044600961031     O                       SUMNFE
044700961009     OEDSUM000  E            AGGERR
044800961009     O                       SUMSTS
044900980324     OEDSUM005  E            AGGAN5
045000980324     O                       SUMFLG
045100980324     O                       SUMNFE
045200980324     OEDSUM005  E            AGGER5
045300980324     O                       SUMSTS
045400961015     OEDBRV000  E            AGGBRV
045500961030     O                       BRVAAS
045600961030     O                       BRVLNP
045700961030     O                       BRVNSP
045800961015     O                       BRVFLG
045900980409     OEDBRV000  E            AGGBR2
046000980409     O                       BRVFLG
046100980409     OEDBRVECC  E            AGGBR3
046200980409     O                       BRVFLG
