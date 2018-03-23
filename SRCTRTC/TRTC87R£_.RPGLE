000100961003     H DECEDIT('0,') DATEDIT(*DMY.)
000200961003     H*---------------------------------------------------------------------*
000300961003     H*  Ritorno date arrivo ai partner  - E.D.I. - Estero -                *
000400961003     F*---------------------------------------------------------------------*
000500961003     F*  DATA BASE                                                          *
000600961003     F*---------------------------------------------------------------------*
000700961003     FCNACO00F  IF   E           K DISK
000800961003     F*---------
000900961003     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
001000961009     F*---------
001100020702     F*ABEL00F  IF   E           K DISK
001200961003     F*---------
001300961003     FEDTAB01L  IF   E           K DISK
001400961003     F*---------
001500961003     FFNFVV01L  IF   E           K DISK
001600961003     F*---------
001700961003     FEDBRV01L  IF   E           K DISK
001800980326     F*---------
001900980326     FEDBRV04L  IF   E           K DISK
002000980326     F                                     RENAME(EDBRV000:EDBRV004)
002100961003     F*---------
002200961003     FTRTC87D   CF   E             WORKSTN
002300961003     F                                     SFILE(TC87S02:NRR2)
002400100621     F*---------------------------------------------------------------------------------
002500100621      * Per Controllare che non ci siano le trasmissioni in sede in corso
002600100621      *  durante l'apertura di questo programma si scrive un record su questo file.
002700100621     Ffiwalc1l  UF A E           K DISK    commit
002800961003     D*---------------------------------------------------------------------*
002900961003     D*  SCHIERE
003000961003     D*---------------------------------------------------------------------*
003100961003     D* Tabelle capiconto
003200020916     D* caricamento £1
003300020702     D £1              S              3  0 DIM(30)
003400961003     D* Tabella partner
003500110712     D CPT             S             35    DIM(999)                             Ident. Partner
003600110712     D KSC             S              7  0 DIM(999)                             ns.codice
003700110712     D DPU             S             90    DIM(999)                             dati tab.PU
003800110712     D LNP             S              3  0 DIM(999)                             COD lnp partner
003900110712     D KCL             S              7  0 DIM(999)                             ns.codice
004000110712     D KPT             S              7  0 DIM(999)                             ns.codice
004100961003     D* Schiera errori
004200980324     D ERR             S             70    DIM(10) CTDATA PERRCD(1)
004300961003     D*---------------------------------------------------------------------*
004400961003     D* DS
004500961003     D*---------------------------------------------------------------------*
004600961003     D KPJBA         E DS
004700100621     d   KPJBUS        s                   like(KPJBU)
004800961003     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004900961003     D  TCU                  398    697
005000961003     D                                     DIM(50)                              Flg 8 tp.conto
005100961003     D  KCU                  698    847P 0
005200961003     D                                     DIM(50)                              Capiconto
005300961003     D                                     PACKEVEN
005400961003     D CNCR80        E DS
005500020702     d trul06ds      e ds
005600020702     d  lin                    1     90  0 dim(30)
005700961003     D* Ds scomposzione tipo capoconti
005800961003     D TCUDS           DS
005900961003     D  F34                    3      4
006000961003     D  F56                    5      6
006100961003     D DSPT          E DS                  EXTNAME(EDIDSPT)
006200961007     D DSPU          E DS                  EXTNAME(EDIDSPU)
006300020726     D DSCL          E DS                  EXTNAME(EDIDSCL)
006400100621      *
006500100621     D ddatiute      e ds                  prefix(UT_)
006600100621     D azuteds       e ds                  extname(AZUTE00F)
006700100621     D tibs34ds      E DS                  inz
006800100621     d dataoggi        s               d   datfmt(*iso)
006900100621      *---------------------------------------------------------------------*
007000100621     D TRTC87ds0     E DS
007100961007     D TRTC87        E DS                  EXTNAME(TRTC87DS)
007200100621      *---------------------------------------------------------------------*
007300961003     D*  Ds per inversione data
007400961003     D WLBDA8          DS
007500961003     D  G02DAT                 1      8  0
007600961003     D  G02INV                 9     16  0
007700961003     D  G02ERR                17     17
007800961003     D  G02TGI                18     22  0
007900961008     D*  Ds per reperimento filiale da secondi livelli
008000020506     D*                DS
008100020506     D* WTPPRF                 1      3
008200020506     D* WFGS                   4      6
008300020506     D* WSEI                   1      6
008400961003     D                SDS
008500961003     D  NOMPGM                 1     10
008600961003      *----------------------------------------------------------------
008700961003      *  FILE
008800961003      *----------------------------------------------------------------
008900961003     IAZORG01L  AA
009000961008     I                             P    4    5 0ORGFIL
009100961008     I                                  6    6  ORGFVA
009200961003     I                                 14   33  ORGDES
009300961003     C*---------------------------------------------------------------------*
009400961003     C* Ciclo principale
009500961003     C*---------------------------------------------------------------------*
009600100621      *
009700100621     C                   clear                   trtc87ds0
009800100621     c                   eval      FN0PGM  = 'TRTC87R'
009900100621     c                   eval      FN0TIPO = 'FILIALE'
010000100621     c                   eval      FN0CODICE = %editc(UTEFIL:'X')
010100100621     C                   MOVEL     KPJBU         kpjbus
010200100621     C                   clear                   kpjbu
010300100621     C                   MOVEL     trtc87ds0     kpjbu
010400100621     c                   call      'TRTC87R0'
010500100621     c                   parm                    kpjba
010600100621     C                   MOVEL     kpjbu         TRTC87ds0
010700100621     C*   se già utilizzata da altro utente della stessa filiale fa uscire
010800100621     C*   forzatamente.
010900100621     c                   if        Fn0FLG <> *blank
011000100621     c                   goto      FINE
011100100621     c                   end
011200100621     C*
011300100621     C                   MOVEL     KPJBUs        kpjbu
011400100621     C*
011500100621      *
011600961003     C* Inizializzo prima videata
011700961003     C                   EXSR      INZ01
011800961003      * Loop gestione videate
011900961003     C     WFINE         DOWEQ     'N'
012000961008     C     WTPVID        CASEQ     '1'           GESD01                         Sce.cliente
012100961008     C     WTPVID        CASEQ     '2'           GESS02                         Subfile
012200961003     C                   END
012300961003     C                   END
012400961003      * Fine Lavoro
012500961003     C     FINE          TAG
012600100621     C*
012700100621     C*  deve cancellare il record di allocazione x impedire le trasmissioni
012800100621     C*   in sede durante l'utilizzo di questo programma.
012900100621     C     Kwalc         setll     fiwalc1l
013000100621     C     Kwalc         reade     fiwalc1l
013100100621     c                   dow       not %eof(fiwalc1l)
013200100621     c                   delete    fiwalc00
013300100621     C     Kwalc         reade     fiwalc1l
013400100621     c                   end
013500100621      *
013600100621     c                   commit
013700100621      *
013800961003     C                   SETON                                        LR
013900961003     C*---------------------------------------------------------------*
014000961003     C*  INZ01: Inizializzo prima videata
014100961003     C*---------------------------------------------------------------*
014200961003     C     INZ01         BEGSR
014300961003     C*
014400980320     C                   SETOFF                                       404142
014500980320     C                   SETOFF                                       1828
014600961003     C                   MOVEL     *BLANKS       $MSG
014700961003     C                   Z-ADD     0             V1CCCM
014800980320     C                   Z-ADD     0             V1CLNP
014900961009     C                   MOVEL     '1'           WTPVID            1
015000961003     C                   MOVEL     NOMPGM        V1CPGM
015100961003     C                   MOVEL     *BLANKS       V1DCCM
015200980320     C                   MOVEL     *BLANKS       V1DLNP
015300961003     C                   MOVEL     *BLANKS       V1DFGS
015400961003     C*  Decodifico filiale in gestione
015500961008     C                   MOVEL     *BLANKS       ORGDES
015600961003     C                   Z-ADD     V1CFGS        ORGFIL
015700961003     C     ORGFIL        CHAIN     AZORG01L                           31
015800961003     C  N31              MOVEL     ORGDES        V1DFGS
015900961003     C*
016000961003     C                   ENDSR
016100961003     C*---------------------------------------------------------------*
016200961003     C*  INZ02: Inizializzo seconda videata
016300961003     C*---------------------------------------------------------------*
016400961003     C     INZ02         BEGSR
016500961003     C*
016600961003     C                   SETOFF                                       28
016700961003     C                   MOVEL     *BLANKS       $MSG
016800961003     C                   MOVEL     NOMPGM        V2CPGM
016900961008     C                   MOVEL     '2'           WTPVID
017000961008     C                   Z-ADD     V1CCCM        V2CCCM
017100961008     C                   MOVEL     V1DCCM        V2DCCM
017200980401     C                   Z-ADD     V1CLNP        V2CLNP
017300980401     C                   MOVEL     V1DLNP        V2DLNP
017400961011     C                   MOVEL     V1CFGS        V2CFGS
017500961003     C*  Pulisco subfile
017600961009     C                   Z-ADD     0             NRR2              4 0
017700961003     C                   SETOFF                                       2021
017800961003     C                   WRITE     TC87C02
017900961003     C                   SETON                                        2021
018000980326     C*  se ho scelto di lavorare per lnp accendo 06  altrimenti 05
018100980326     C     V1CLNP        COMP      0                                  060605
018200961007      *  Eseguo caricamento in base al cod.cliente prima videata
018300961008     C                   Z-ADD     0             NRR2
018400961008      *  Prima lettura:
018500980326     C     V1CLNP        IFEQ      0
018600961008     C     KCCM          CHAIN     EDBRV01L                           30
018700980326     C                   ELSE
018800980326     C     KLNP          CHAIN     EDBRV04L                           30
018900980326     C                   END
019000961008     C     *IN30         DOWEQ     '0'                                          Eof EDBRV or
019100961008     C                   EXSR      ROLLUP
019200961008     C                   END
019300961003     C*
019400961003     C                   ENDSR
019500961003     C*---------------------------------------------------------------*
019600961003     C*  GESD01: Gestione prima videata
019700961003     C*---------------------------------------------------------------*
019800961003     C     GESD01        BEGSR
019900961003     C*
020000961008     C                   EXFMT     TC87D01
020100961008     C                   SETOFF                                       284041
020200961003     C*  Fine Lavoro
020300961003     C     *INKC         IFEQ      '1'
020400961003     C                   MOVEL     'S'           WFINE
020500961003     C                   GOTO      FINVD1
020600961003     C                   END
020700961008     C*  Cambio filiale
020800961008     C     *INKS         IFEQ      '1'
020900961008     C                   SETON                                        18
021000961008     C                   GOTO      FINVD1
021100961008     C                   END
021200961003     C*  Controlli
021300961003     C                   EXSR      CTR01
021400961007     C   28              GOTO      FINVD1
021500961007     C*  Caricamento subfile
021600961007     C                   EXSR      INZ02
021700961003     C*
021800961003     C     FINVD1        ENDSR
021900961003     C*---------------------------------------------------------------*
022000961003     C*  GESS02: Gestione subfile pieno
022100961003     C*---------------------------------------------------------------*
022200961003     C     GESS02        BEGSR
022300961003     C*
022400961009     C                   WRITE     TC87T02
022500961009     C                   EXFMT     TC87C02
022600961003     C*  Fine Lavoro
022700961003     C     *INKC         IFEQ      '1'
022800961003     C                   MOVEL     'S'           WFINE
022900961003     C                   GOTO      FINVS2
023000961003     C                   END
023100961003     C*  Ritorno
023200961003     C     *INKL         IFEQ      '1'
023300961003     C                   EXSR      INZ01
023400961003     C                   GOTO      FINVS2
023500961003     C                   END
023600961003     C*  Controlli
023700961003     C                   EXSR      CTR02
023800961003     C*
023900961003     C     FINVS2        ENDSR
024000961003     C*---------------------------------------------------------------*
024100961003     C*  CTR01: Controlli prima videata
024200961003     C*---------------------------------------------------------------*
024300961003     C     CTR01         BEGSR
024400961003     C*
024500961008     C*  Filiale in gestione: controllo se esiste
024600961008     C     V1CFGS        IFNE      WSVFGS
024700961009     C                   Z-ADD     V1CFGS        ORGFIL
024800961008     C     ORGFIL        CHAIN     AZORG01L                           30
024900961008     C     *IN30         IFEQ      '1'
025000961011     C     ORGFVA        ORNE      *BLANK
025100961008     C                   SETON                                        4028
025200961008     C                   MOVEL     ERR(1)        $MSG
025300961011     C                   GOTO      FINCT1
025400961008     C                   END
025500961008     C                   MOVEL     ORGDES        V1DFGS
025600961008     C*  ... controllo se gestita su questo as
025700961008     C     V1CFGS        LOOKUP    £1                                     32
025800961008     C     *IN32         IFEQ      '0'
025900961008     C                   SETON                                        4028
026000961008     C                   MOVEL     ERR(2)        $MSG
026100961011     C                   GOTO      FINCT1
026200961008     C                   END
026300980113     C                   END
026400980320     C*  Devo selezionare o la linea di partenza di un partner o un
026500980320     C*  codice cliente
026600980320     C     V1CCCM        IFEQ      0
026700980320     C     V1CLNP        ANDEQ     0
026800980320     C                   SETON                                        424128
026900980320     C                   MOVEL     ERR(8)        $MSG
027000980320     C                   GOTO      FINCT1
027100980320     C                   END
027200980320     C* ... ma non entrambi
027300980320     C     V1CCCM        IFNE      0
027400980320     C     V1CLNP        ANDNE     0
027500980320     C                   SETON                                        424128
027600980320     C                   MOVEL     ERR(9)        $MSG
027700980320     C                   GOTO      FINCT1
027800980320     C                   END
027900980320     C*  Controllo se cliente esiste su CNACO
028000980320     C     V1CCCM        IFGT      0
028100961003     C                   Z-ADD     1             KKUT
028200961003     C                   Z-ADD     KCI           KKCC
028300961003     C                   Z-ADD     V1CCCM        KKSC
028400961008     C                   MOVEL     *BLANKS       ACORAG
028500961003     C     KACO          CHAIN     CNACO00F                           30
028600961003     C     *IN30         IFEQ      '1'
028700961003     C     ACOFLG        ORNE      ' '
028800961008     C                   SETON                                        2841
028900961008     C                   MOVEL     ERR(4)        $MSG
029000961003     C                   GOTO      FINCT1
029100961003     C                   END
029200961104     C                   MOVE      V1CCCM        WCCN              7 0
029300961009     C                   Z-ADD     1             X
029400961008     C                   MOVEL     ACORAG        V1DCCM
029500961009     C     V1CCCM        LOOKUP    KSC(X)                                 32
029600020726      *
029700020726      *  Se non era presente sulla PT prova anche sull CL altrimenti segnala
029800020726      *   l'errore.
029900020726     C     *IN32         IFEQ      '0'
030000020726     C                   Z-ADD     1             XX                3 0
030100020726     C     V1CCCM        LOOKUP    KCL(XX)                                32
030200020726     C   32              Z-ADD     1             X
030300020726     C   32KPT(XX)       LOOKUP    KSC(X)                                 32
030400020726     C                   END
030500020221      *
030600961029     C*  NO Codice tabella PT
030700961029     C     *IN32         IFEQ      '0'
030800961008     C                   SETON                                        2841
030900961008     C                   MOVEL     ERR(5)        $MSG
031000961003     C                   GOTO      FINCT1
031100961003     C                   END
031200961009     C*  Controllo se devo trasmettere le date arrivo al cliente
031300961009     C                   MOVEL     DPU(X)        DSPU
031400961009     C     §PUDTA        IFEQ      ' '
031500961009     C                   SETON                                        2841
031600961009     C                   MOVEL     ERR(7)        $MSG
031700961009     C                   GOTO      FINCT1
031800961009     C                   END
031900961008     C*  Controllo se ha date di arrivo da spedire
032000961008     C                   Z-ADD     V1CCCM        KCCM
032100961008     C     KCCM          CHAIN     EDBRV01L                           30
032200961029     C     *IN30         IFEQ      '1'
032300961008     C                   SETON                                        2841
032400961008     C                   MOVEL     ERR(6)        $MSG
032500961008     C                   GOTO      FINCT1
032600961003     C                   END
032700980320     C*
032800980326     C                   ELSE
032900980326     C                   Z-ADD     V1CLNP        ORGFIL
033000980401     C                   MOVEL     *BLANKS       V1DLNP
033100980326     C     ORGFIL        CHAIN     AZORG01L                           30
033200980326     C     *IN30         IFEQ      '1'
033300980326     C     ORGFVA        ORNE      *BLANK
033400980326     C                   SETON                                        2842
033500980326     C                   MOVEL     ERR(01)       $MSG
033600980326     C                   GOTO      FINCT1
033700980401     C*  Decodifico LNP
033800980401     C                   ELSE
033900980401     C                   MOVEL     ORGDES        V1DLNP
034000980326    2C                   ENDIF
034100980326     C*  se richiesta elaborazione x lnp ctrl se è un partner
034200980326     C                   Z-ADD     1             YY                3 0
034300980326     C     V1CLNP        LOOKUP    LNP(YY)                                30
034400980326    2C     *IN30         IFEQ      *OFF
034500980326     C                   SETON                                        2842
034600980326     C                   MOVEL     ERR(10)       $MSG
034700980326     C                   GOTO      FINCT1
034800980326    2C                   ENDIF
034900980326     C*  Controllo se devo trasmettere le date arrivo
035000980401     C                   MOVEL     DPU(YY)       DSPU
035100980326     C     §PUDTA        IFEQ      ' '
035200980326     C                   SETON                                        2842
035300980326     C                   MOVEL     ERR(7)        $MSG
035400980326     C                   GOTO      FINCT1
035500980326     C                   END
035600980326     C*  Controllo se ha date di arrivo da spedire
035700980326     C                   Z-ADD     V1CLNP        KLNP
035800980326     C     KLNP          CHAIN     EDBRV04L                           30
035900980326     C     *IN30         IFEQ      '1'
036000980326     C                   SETON                                        2842
036100980326     C                   MOVEL     ERR(6)        $MSG
036200980326     C                   GOTO      FINCT1
036300980326     C                   END
036400980326     C*
036500980320     C                   END
036600961003     C*
036700961003     C     FINCT1        ENDSR
036800961003     C*---------------------------------------------------------------*
036900961003     C*  CTR02: Controlli seconda videata
037000961003     C*---------------------------------------------------------------*
037100961003     C     CTR02         BEGSR
037200961003     C*
037300970228     C                   MOVEL     'N'           WCANC
037400961011     C                   CLEAR                   TRTC87
037500961003     C*  Controllo selezioni effettuate
037600961008     C                   READC     TC87S02                                29
037700961003     C     *IN29         DOWEQ     '0'
037800961011     C     D87CMD        ANDNE     '03'
037900970228     C     V2CSCE        IFNE      ' '
038000961003     C*  Richiamo pgm per esecuzione scrittura VAB
038100961008     C                   CLEAR                   TRTC87
038200980401     C                   MOVEL     §PUBS1        D87BS1
038300980409     C                   MOVEL     §PUECC        D87SEC
038400961008     C                   Z-ADD     V2CNFE        D87NFE
038500961008     C                   MOVEL     V2CDFE        G02DAT
038600961008     C                   Z-ADD     0             G02INV
038700961008     C                   MOVEL     *BLANKS       G02ERR
038800980320     C                   CALL      'XSRDA8'
038900961008     C                   PARM                    WLBDA8
039000961008     C                   Z-ADD     G02INV        D87DFE
039100961008     C                   Z-ADD     V2CCCM        D87CCM
039200980320     C                   Z-ADD     V2CLNP        D87LNP
039300961104     C                   Z-ADD     WCCN          D87CCN
039400970228     C     V2CSCE        IFEQ      '1'
039500961009     C                   MOVEL     V2DCCM        D87DEC
039600980401     C                   MOVEL     V2DLNP        D87DLP
039700961009     C                   MOVEL     V2CERR        D87ERR
039800961024     C                   MOVEL     'S'           D87WRT
039900980320     C                   MOVEL     V2CRIT        D87RIT
040000961008     C                   MOVEL     TRTC87        KPJBU
040100961011      *  Se ho avuto errori in precedenti elab. sfleggo dati
040200961011     C                   CALL      'TRTC87R5'
040300961011     C                   PARM                    KPJBA
040400961008      *  Richiamo pgm x per aggiornamento archivio errori in
040500961008      *  ricezione bolle
040600961008     C                   CALL      'TRTC87R1'
040700961008     C                   PARM                    KPJBA
040800961008      *  Richiamo pgm gestione videata errori su foglio entrata
040900961008     C                   CALL      'TRTC87R2'
041000961008     C                   PARM                    KPJBA
041100961008     C                   MOVEL     KPJBU         TRTC87
041200970228     C                   END
041300970228     C* Call a pgm che cancella foglio entrata
041400970305     C   KQV2CSCE        IFEQ      '4'
041500970228     C                   MOVEL     TRTC87        KPJBU
041600970228     C                   CALL      'TRTC87R4'
041700970228     C                   PARM                    KPJBA
041800970228     C                   MOVEL     'S'           WCANC             1
041900970228     C                   END
042000961008      *  Aggiorno record subfile
042100961008     C                   MOVE      V2CIN2        *IN02
042200961008     C                   MOVEL     *BLANKS       V2CSCE
042300961008     C                   UPDATE    TC87S02
042400970228     C                   END
042500961008     C*  Lettura successiva subfile
042600961008     C                   READC     TC87S02                                29
042700961003     C                   END
042800961011     C*
042900961011      *  F12=Sfleggo record annullati e riemetto videata
043000961011     C     D87CMD        IFEQ      '12'
043100961011     C                   CALL      'TRTC87R5'
043200961011     C                   PARM                    KPJBA
043300961011     C                   EXSR      INZ01
043400961011     C                   END
043500961011     C*
043600961011     C*  F6=Riemetto prima videata
043700961011     C     D87CMD        IFEQ      '06'
043800970228     C     WCANC         OREQ      'S'
043900961008     C                   EXSR      INZ01
044000961008     C                   END
044100961011     C*
044200961011     C     D87CMD        IFEQ      '03'
044300961011      *  F3=Sfleggo record annullati
044400961011     C                   CALL      'TRTC87R5'
044500961011     C                   PARM                    KPJBA
044600961011     C                   MOVEL     'S'           WFINE
044700961011     C                   END
044800961003     C*
044900961003     C                   ENDSR
045000961003     C*----------------------------------------------------*
045100961003     C*   ROLLUP: Caricamento dati da EDVAB                *
045200961003     C*----------------------------------------------------*
045300961003     C     ROLLUP        BEGSR
045400961003     C*
045500961008     C                   CLEAR                   TC87S02
045600961008     C                   MOVEL     *BLANKS       V2CSCE
045700961008      *  Eseguo posizionamento su foglio entrata
045800961008     C                   MOVEL     '5'           KNPG
045900961008     C                   Z-ADD     BRVNFE        KNFV
046000961008     C                   Z-ADD     V1CFGS        KFGS
046100961008     C     KFVV          CHAIN     FNFVV01L                           31
046200961008     C                   Z-ADD     FVVNFV        V2CNFE
046300961008     C                   MOVEL     FVVDSF        V2DNFE
046400961008     C                   Z-ADD     FVVDFV        G02INV
046500961008     C                   Z-ADD     0             G02DAT
046600961008     C                   MOVEL     '3'           G02ERR
046700961008     C                   CALL      'XSRDA8'
046800961008     C                   PARM                    WLBDA8
046900961008     C                   Z-ADD     G02DAT        V2CDFE
047000961008     C                   MOVEL     'S'           V2CERR
047100980320     C                   MOVEL     ' '           V2CRIT
047200961008      *  Se scrivo EDBRV con i soli errori (flag tab.PT) non
047300961008      *  permetto che mi selezionino tutto
047400961008     C     §PUDTB        IFEQ      'E'
047500961008     C                   SETOFF                                       02
047600961008     C                   ELSE
047700961008     C                   SETON                                        02
047800961008     C                   END
047900961008     C                   MOVEL     *IN02         V2CIN2
048000961008     C                   ADD       1             NRR2
048100961008     C                   WRITE     TC87S02
048200980326     C*  Mi posiziono sul prossimo Foglio Entrata
048300961008     C                   MOVEL     BRVNFE        KNFE
048400980326     C     V1CLNP        IFEQ      0
048500961008     C     KBRV          SETGT     EDBRV01L
048600961008     C     KCCM          READE     EDBRV01L                               30
048700980326     C                   ELSE
048800980326     C     KBRV1         SETGT     EDBRV04L
048900980326     C     KLNP          READE     EDBRV04L                               30
049000980326     C                   END
049100961003     C*
049200961003     C                   ENDSR
049300961003     C*----------------------------------------------------*
049400961003     C*   *INZSR: Operazioni iniziali                      *
049500961003     C*----------------------------------------------------*
049600961010     C     *INZSR        BEGSR
049700961003     C*
049800961003     C     *ENTRY        PLIST
049900961003     C                   PARM                    KPJBA
050000100621      *
050100100621     c     *dtaara       define    §azute        azuteds
050200100621     c     *dtaara       define    §datiute      ddatiute
050300100621     C                   in(E)     *dtaara
050400100621     C                   IF        %Error  or  RSUT = *blanks
050500100621     C                   call      'TIBS34R'
050600100621     C                   parm                    Tibs34Ds
050700100621     C                   in        *dtaara
050800100621     c                   ENDIF
050900961003     C*  Richiamo XPARUT
051000961003     C                   Z-ADD     1             CODUT
051100961003     C                   CALL      'X§PARUT'
051200961003     C                   PARM                    UT§DSE
051300961003     C                   MOVEL     REC80         CNCR80
051400961003     C*  Ricerca capoconti
051500961003     C                   DO        50            X
051600961003     C                   MOVE      TCU(X)        TCUDS
051700961003     C     F56           IFEQ      'CG'
051800961003     C     F34           ANDEQ     '01'
051900961003     C                   Z-ADD     KCU(X)        KCI               4 0
052000961003     C                   END
052100961003     C                   END
052200961003     C                   MOVEL     RAGUT         RSUT
052300961003     C*  Chiavi di accesso
052400961003     C     KACO          KLIST
052500961003     C                   KFLD                    KKUT
052600961003     C                   KFLD                    KKCC
052700961003     C                   KFLD                    KKSC
052800020702     C*    KTAB1         KLIST
052900020702     C*                  KFLD                    KKUT
053000020702     C*                  KFLD                    KCOD1
053100020702     C*    KTAB2         KLIST
053200020702     C*                  KFLD                    KKUT
053300020702     C*                  KFLD                    KCOD1
053400020702     C*                  KFLD                    KKEY
053500961008     C     KBRV          KLIST
053600961008     C                   KFLD                    KCCM
053700961008     C                   KFLD                    KNFE
053800980326     C     KBRV1         KLIST
053900980326     C                   KFLD                    KLNP
054000980326     C                   KFLD                    KNFE
054100961008     C     KFVV          KLIST
054200961008     C                   KFLD                    KNPG
054300961008     C                   KFLD                    KNFV
054400961008     C                   KFLD                    KFGS
054500100621      *
054600100621     C* DEFINIZIONE CHIAVI DI ACCESSO
054700100621     C     Kwalc         KLIST
054800100621     C                   KFLD                    WalPGM
054900100621     C                   KFLD                    WalNOJ
055000100621     C                   KFLD                    WalNUS
055100100621     C                   KFLD                    WalNRJ
055200100621      *
055300961003     C*  Definizione variabili
055400961003     C     *LIKE         DEFINE    ACOKUT        KKUT
055500961003     C     *LIKE         DEFINE    ACOKCC        KKCC
055600961003     C     *LIKE         DEFINE    ACOKSC        KKSC
055700961003     C     *LIKE         DEFINE    TABCOD        KCOD
055800961008     C     *LIKE         DEFINE    BRVCCM        KCCM
055900980326     C     *LIKE         DEFINE    BRVLNP        KLNP
056000961008     C     *LIKE         DEFINE    BRVNFE        KNFE
056100961008     C     *LIKE         DEFINE    FVVNPG        KNPG
056200961008     C     *LIKE         DEFINE    FVVNFV        KNFV
056300961008     C     *LIKE         DEFINE    FVVFGS        KFGS
056400020702     C*    *LIKE         DEFINE    TBLCOD        KCOD1
056500020702     C*    *LIKE         DEFINE    TBLKEY        KKEY
056600961009     C     *LIKE         DEFINE    V1CFGS        WSVFGS
056700961008     C*  Reperisco filiale gestione in base al profilo utente
056800961008     C                   SETOFF                                       01
056900020506     c     simtpp        ifeq      '2'
057000020506     c     simtpp        oreq      *blanks
057100020506     c                   movel     simpou        v1cfgs
057200961008     C                   ELSE
057300961008     C                   SETON                                        01
057400961008     C                   Z-ADD     SIMFEL        V1CFGS
057500961008     C                   END
057600961009     C                   Z-ADD     V1CFGS        WSVFGS
057700020916     C*  Carico £1
057800020916     c                   clear                   trul06ds
057900020916     c                   eval      d06cod = '£1'
058000020916     c                   movel     simfel        d06key
058100020916     c                   movel     trul06ds      kpjbu
058200020916     c                   call      'TRUL06R'
058300020916     c                   parm                    kpjba
058400020916     c                   movel     kpjbu         trul06ds
058500020916     c                   movea     lin           £1
058600020916     C                   Z-ADD     1             KKUT
058700020916      *
058800961003     C*  Carico in schiera codici tabella - PT -
058900020702     C                   Z-ADD     0             X                 3 0
059000961003     C                   Z-ADD     0             KSC
059100961003     C                   MOVEL     'PT'          KCOD
059200961003     C     KCOD          CHAIN     EDTAB01L                           31
059300961003     C     *IN31         DOWEQ     '0'
059400961003     C     TABFLG        IFEQ      ' '
059500020916     C                   MOVEL     TABUNI        DSPT
059600020916      * solo se compresa nelle linee di partenza del terminal
059700020916     c     §ptlnp        lookup    £1                                     32
059800020916     c                   if        *in32 = *on
059900961003     C                   ADD       1             X
060000961011     C                   MOVEL     TABKEY        CPT(X)
060100961003     C                   Z-ADD     §PTKSC        KSC(X)
060200980320     C                   MOVEL     §PTLNP        LNP(X)
060300020916     c                   end
060400961003     C                   END
060500961003     C     KCOD          READE     EDTAB01L                               31
060600961003     C                   END
060700961008     C*  Carico continuazione
060800961008     C                   MOVEA     *BLANKS       DPU
060900961008     C                   MOVEL     'PU'          TABCOD
061000961008     C     TABCOD        CHAIN     EDTAB01L                           30
061100961008     C     *IN30         DOWEQ     '0'
061200961008     C     TABFLG        IFEQ      ' '
061300961008     C                   MOVEL     TABKEY        CODPU            35
061400961008     C                   Z-ADD     1             X
061500961008     C     CODPU         LOOKUP    CPT(X)                                 32
061600961008     C     *IN32         IFEQ      '1'
061700961008     C                   MOVEL     TABUNI        DPU(X)
061800980324     C                   MOVEL     TABUNI        DSPU
061900980320     C     §PUTPC        IFNE      'P'
062000980320     C                   Z-ADD     0             LNP(X)
062100980320     C                   ENDIF
062200030715     C****               ELSE
062300030715     C****               Z-ADD     0             LNP(X)
062400980320     C                   ENDIF
062500980324     C                   ENDIF
062600961008     C     TABCOD        READE     EDTAB01L                               30
062700961008     C                   END
062800020726     C*  Carico in schiera codici collegati - CL -
062900020726     C                   Z-ADD     0             X                 3 0
063000020726     C                   Z-ADD     0             KCL
063100020726     C                   Z-ADD     0             KPT
063200020726     C                   MOVEL     'CL'          KCOD
063300020726     C     KCOD          CHAIN     EDTAB01L                           31
063400020726     C     *IN31         DOWEQ     '0'
063500020726     C     TABFLG        IFEQ      ' '
063600020726     C                   ADD       1             X
063700020726     C                   MOVEL     TABUNI        DSCL
063800020726     C                   MOVEL     TABKEY        KPT(X)
063900020726     C                   Z-ADD     §CLKSC        KCL(X)
064000020726     C                   END
064100020726     C     KCOD          READE     EDTAB01L                               31
064200020726     C                   END
064300961003     C*  Inizializzo variabili
064400961003     C                   MOVEL     'N'           WFINE             1
064500100621      ***
064600100621     C                   TIME                    W0140            14 0
064700100621     C                   MOVEl     W0140         Wora              6 0
064800100621     C                   MOVE      W0140         WDAT              8 0
064900100621     C*
065000100621     C                   Z-ADD     Wdat          G02DAT
065100100621     C                   MOVEL     *BLANK        G02ERR
065200100621     C                   CALL      'XSRDA8'
065300100621     C                   PARM                    WLBDA8
065400100621     C* UDATE A 8 IN AAAA/MM/GG
065500100621     C                   move      G02INV        dataoggi
065600100621     C*
065700100621     C* scrive il record di allocazione x trasmissione
065800100621     C                   clear                   Fiwalc00
065900100621     C                   movel     w0140         walOrc
066000100621     C                   movel     g02inv        waldac
066100100621     C                   eval      WalSIF = KNSIF
066200100621     C                   eval      WalPGM = 'TRTC87R   '
066300100621     C                   eval      WalNOJ = KNMEB
066400100621     C                   eval      WalNUS = KNMUS
066500100621     C                   eval      WalNRJ = KNRJO
066600100621     C                   eval      WalFLD = 'FILIALE   ' + %editc(UTEFIL:'X')
066700100622     c                                      + ' - ATR'
066800100621     C                   write     Fiwalc00
066900100621     c                   feod      Fiwalc1L
067000100621     C***
067100961003     C*
067200961003     C                   ENDSR
067300980320**
067400961008Punto operativo inesistente o annullato                                01
067500961008Punto operativo non gestito su questo AS                               02
067600961008Il punto operativo non si gestisce in modo autonomo                    03
067700961008Codice cliente inesistente o annullato                                 04
067800961008Codice cliente non relativo ad un partner                              05
067900980401Per cliente/Lnp selezionato non ci sono date di arrivo da trasmettere  06
068000980401Per cliente/Lnp selezionato non devo trasmettere le date di arrivo     07
068100980320Selezionare un codice cliente o un linea di partenza di un partner     08
068200980320Impossibile selezionare entrambi (sia lnp che codice cliente)          09
068300980320La Lnp non esiste su tabella EDI o non appartiene ad un partner        10
