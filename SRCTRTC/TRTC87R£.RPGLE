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
029200130301     C*-----****
029300130301      ** occorre controllare che la filiale del codice cliente appartenga
029400130301      **  al terminal selezionato
029500130301     c                   movel     v1cccm        Filiale_CLI       3 0
029600130301     C     Filiale_CLI   LOOKUP    £1                                     32
029700130301     C     *IN32         IFEQ      '0'
029800130301      **
029900130301      **  controlla sulla CL se esiste un legame e se c'è controlla con quello
030000130301     C                   Z-ADD     1             XX
030100130301     C     V1CCCM        LOOKUP    KCL(XX)                                32
030200130301      **
030300130301     C   32              movel     KPT(XX)       Filiale_CLI
030400130301     C   32Filiale_CLI   LOOKUP    £1                                     32
030500130301      **
030600130301     C     *IN32         IFEQ      '0'
030700130301     C                   SETON                                          4128
030800130301     C                   MOVEL     ERR(2)        $MSG
030900130301     C                   GOTO      FINCT1
031000130301     C                   END
031100130301      **
031200130301     C                   END
031300130301     C*-----****
031400961104     C                   MOVE      V1CCCM        WCCN              7 0
031500961009     C                   Z-ADD     1             X
031600961008     C                   MOVEL     ACORAG        V1DCCM
031700961009     C     V1CCCM        LOOKUP    KSC(X)                                 32
031800020726      *
031900020726      *  Se non era presente sulla PT prova anche sull CL altrimenti segnala
032000020726      *   l'errore.
032100020726     C     *IN32         IFEQ      '0'
032200020726     C                   Z-ADD     1             XX                3 0
032300020726     C     V1CCCM        LOOKUP    KCL(XX)                                32
032400020726     C   32              Z-ADD     1             X
032500020726     C   32KPT(XX)       LOOKUP    KSC(X)                                 32
032600020726     C                   END
032700020221      *
032800961029     C*  NO Codice tabella PT
032900961029     C     *IN32         IFEQ      '0'
033000961008     C                   SETON                                        2841
033100961008     C                   MOVEL     ERR(5)        $MSG
033200961003     C                   GOTO      FINCT1
033300961003     C                   END
033400961009     C*  Controllo se devo trasmettere le date arrivo al cliente
033500961009     C                   MOVEL     DPU(X)        DSPU
033600961009     C     §PUDTA        IFEQ      ' '
033700961009     C                   SETON                                        2841
033800961009     C                   MOVEL     ERR(7)        $MSG
033900961009     C                   GOTO      FINCT1
034000961009     C                   END
034100961008     C*  Controllo se ha date di arrivo da spedire
034200961008     C                   Z-ADD     V1CCCM        KCCM
034300961008     C     KCCM          CHAIN     EDBRV01L                           30
034400961029     C     *IN30         IFEQ      '1'
034500961008     C                   SETON                                        2841
034600961008     C                   MOVEL     ERR(6)        $MSG
034700961008     C                   GOTO      FINCT1
034800961003     C                   END
034900130301      **
035000980326     C                   ELSE
035100130301      **
035200980326     C                   Z-ADD     V1CLNP        ORGFIL
035300980401     C                   MOVEL     *BLANKS       V1DLNP
035400980326     C     ORGFIL        CHAIN     AZORG01L                           30
035500980326     C     *IN30         IFEQ      '1'
035600980326     C     ORGFVA        ORNE      *BLANK
035700980326     C                   SETON                                        2842
035800980326     C                   MOVEL     ERR(01)       $MSG
035900980326     C                   GOTO      FINCT1
036000980401     C*  Decodifico LNP
036100980401     C                   ELSE
036200130301     C*-----****
036300130301     c                   movel     v1clnp        Filiale_CLI       3 0
036400130301     C     Filiale_CLI   LOOKUP    £1                                     32
036500130301     C     *IN32         IFEQ      '0'
036600130301     C                   SETON                                        42  28
036700130301     C                   MOVEL     ERR(2)        $MSG
036800130301     C                   GOTO      FINCT1
036900130301     C                   END
037000130301     C*-----****
037100980401     C                   MOVEL     ORGDES        V1DLNP
037200980326    2C                   ENDIF
037300980326     C*  se richiesta elaborazione x lnp ctrl se è un partner
037400980326     C                   Z-ADD     1             YY                3 0
037500980326     C     V1CLNP        LOOKUP    LNP(YY)                                30
037600980326    2C     *IN30         IFEQ      *OFF
037700980326     C                   SETON                                        2842
037800980326     C                   MOVEL     ERR(10)       $MSG
037900980326     C                   GOTO      FINCT1
038000980326    2C                   ENDIF
038100980326     C*  Controllo se devo trasmettere le date arrivo
038200980401     C                   MOVEL     DPU(YY)       DSPU
038300980326     C     §PUDTA        IFEQ      ' '
038400980326     C                   SETON                                        2842
038500980326     C                   MOVEL     ERR(7)        $MSG
038600980326     C                   GOTO      FINCT1
038700980326     C                   END
038800980326     C*  Controllo se ha date di arrivo da spedire
038900980326     C                   Z-ADD     V1CLNP        KLNP
039000980326     C     KLNP          CHAIN     EDBRV04L                           30
039100980326     C     *IN30         IFEQ      '1'
039200980326     C                   SETON                                        2842
039300980326     C                   MOVEL     ERR(6)        $MSG
039400980326     C                   GOTO      FINCT1
039500980326     C                   END
039600980326     C*
039700980320     C                   END
039800961003     C*
039900961003     C     FINCT1        ENDSR
040000961003     C*---------------------------------------------------------------*
040100961003     C*  CTR02: Controlli seconda videata
040200961003     C*---------------------------------------------------------------*
040300961003     C     CTR02         BEGSR
040400961003     C*
040500970228     C                   MOVEL     'N'           WCANC
040600961011     C                   CLEAR                   TRTC87
040700961003     C*  Controllo selezioni effettuate
040800961008     C                   READC     TC87S02                                29
040900961003     C     *IN29         DOWEQ     '0'
041000961011     C     D87CMD        ANDNE     '03'
041100970228     C     V2CSCE        IFNE      ' '
041200961003     C*  Richiamo pgm per esecuzione scrittura VAB
041300961008     C                   CLEAR                   TRTC87
041400980401     C                   MOVEL     §PUBS1        D87BS1
041500980409     C                   MOVEL     §PUECC        D87SEC
041600961008     C                   Z-ADD     V2CNFE        D87NFE
041700961008     C                   MOVEL     V2CDFE        G02DAT
041800961008     C                   Z-ADD     0             G02INV
041900961008     C                   MOVEL     *BLANKS       G02ERR
042000980320     C                   CALL      'XSRDA8'
042100961008     C                   PARM                    WLBDA8
042200961008     C                   Z-ADD     G02INV        D87DFE
042300961008     C                   Z-ADD     V2CCCM        D87CCM
042400980320     C                   Z-ADD     V2CLNP        D87LNP
042500961104     C                   Z-ADD     WCCN          D87CCN
042600970228     C     V2CSCE        IFEQ      '1'
042700961009     C                   MOVEL     V2DCCM        D87DEC
042800980401     C                   MOVEL     V2DLNP        D87DLP
042900961009     C                   MOVEL     V2CERR        D87ERR
043000961024     C                   MOVEL     'S'           D87WRT
043100980320     C                   MOVEL     V2CRIT        D87RIT
043200961008     C                   MOVEL     TRTC87        KPJBU
043300961011      *  Se ho avuto errori in precedenti elab. sfleggo dati
043400961011     C                   CALL      'TRTC87R5'
043500961011     C                   PARM                    KPJBA
043600961008      *  Richiamo pgm x per aggiornamento archivio errori in
043700961008      *  ricezione bolle
043800961008     C                   CALL      'TRTC87R1'
043900961008     C                   PARM                    KPJBA
044000961008      *  Richiamo pgm gestione videata errori su foglio entrata
044100961008     C                   CALL      'TRTC87R2'
044200961008     C                   PARM                    KPJBA
044300961008     C                   MOVEL     KPJBU         TRTC87
044400970228     C                   END
044500970228     C* Call a pgm che cancella foglio entrata
044600970305     C   KQV2CSCE        IFEQ      '4'
044700970228     C                   MOVEL     TRTC87        KPJBU
044800970228     C                   CALL      'TRTC87R4'
044900970228     C                   PARM                    KPJBA
045000970228     C                   MOVEL     'S'           WCANC             1
045100970228     C                   END
045200961008      *  Aggiorno record subfile
045300961008     C                   MOVE      V2CIN2        *IN02
045400961008     C                   MOVEL     *BLANKS       V2CSCE
045500961008     C                   UPDATE    TC87S02
045600970228     C                   END
045700961008     C*  Lettura successiva subfile
045800961008     C                   READC     TC87S02                                29
045900961003     C                   END
046000961011     C*
046100961011      *  F12=Sfleggo record annullati e riemetto videata
046200961011     C     D87CMD        IFEQ      '12'
046300961011     C                   CALL      'TRTC87R5'
046400961011     C                   PARM                    KPJBA
046500961011     C                   EXSR      INZ01
046600961011     C                   END
046700961011     C*
046800961011     C*  F6=Riemetto prima videata
046900961011     C     D87CMD        IFEQ      '06'
047000970228     C     WCANC         OREQ      'S'
047100961008     C                   EXSR      INZ01
047200961008     C                   END
047300961011     C*
047400961011     C     D87CMD        IFEQ      '03'
047500961011      *  F3=Sfleggo record annullati
047600961011     C                   CALL      'TRTC87R5'
047700961011     C                   PARM                    KPJBA
047800961011     C                   MOVEL     'S'           WFINE
047900961011     C                   END
048000961003     C*
048100961003     C                   ENDSR
048200961003     C*----------------------------------------------------*
048300961003     C*   ROLLUP: Caricamento dati da EDVAB                *
048400961003     C*----------------------------------------------------*
048500961003     C     ROLLUP        BEGSR
048600961003     C*
048700961008     C                   CLEAR                   TC87S02
048800961008     C                   MOVEL     *BLANKS       V2CSCE
048900961008      *  Eseguo posizionamento su foglio entrata
049000961008     C                   MOVEL     '5'           KNPG
049100961008     C                   Z-ADD     BRVNFE        KNFV
049200961008     C                   Z-ADD     V1CFGS        KFGS
049300961008     C     KFVV          CHAIN     FNFVV01L                           31
049400961008     C                   Z-ADD     FVVNFV        V2CNFE
049500961008     C                   MOVEL     FVVDSF        V2DNFE
049600961008     C                   Z-ADD     FVVDFV        G02INV
049700961008     C                   Z-ADD     0             G02DAT
049800961008     C                   MOVEL     '3'           G02ERR
049900961008     C                   CALL      'XSRDA8'
050000961008     C                   PARM                    WLBDA8
050100961008     C                   Z-ADD     G02DAT        V2CDFE
050200961008     C                   MOVEL     'S'           V2CERR
050300980320     C                   MOVEL     ' '           V2CRIT
050400961008      *  Se scrivo EDBRV con i soli errori (flag tab.PT) non
050500961008      *  permetto che mi selezionino tutto
050600961008     C     §PUDTB        IFEQ      'E'
050700961008     C                   SETOFF                                       02
050800961008     C                   ELSE
050900961008     C                   SETON                                        02
051000961008     C                   END
051100961008     C                   MOVEL     *IN02         V2CIN2
051200961008     C                   ADD       1             NRR2
051300961008     C                   WRITE     TC87S02
051400980326     C*  Mi posiziono sul prossimo Foglio Entrata
051500961008     C                   MOVEL     BRVNFE        KNFE
051600980326     C     V1CLNP        IFEQ      0
051700961008     C     KBRV          SETGT     EDBRV01L
051800961008     C     KCCM          READE     EDBRV01L                               30
051900980326     C                   ELSE
052000980326     C     KBRV1         SETGT     EDBRV04L
052100980326     C     KLNP          READE     EDBRV04L                               30
052200980326     C                   END
052300961003     C*
052400961003     C                   ENDSR
052500961003     C*----------------------------------------------------*
052600961003     C*   *INZSR: Operazioni iniziali                      *
052700961003     C*----------------------------------------------------*
052800961010     C     *INZSR        BEGSR
052900961003     C*
053000961003     C     *ENTRY        PLIST
053100961003     C                   PARM                    KPJBA
053200100621      *
053300100621     c     *dtaara       define    §azute        azuteds
053400100621     c     *dtaara       define    §datiute      ddatiute
053500100621     C                   in(E)     *dtaara
053600100621     C                   IF        %Error  or  RSUT = *blanks
053700100621     C                   call      'TIBS34R'
053800100621     C                   parm                    Tibs34Ds
053900100621     C                   in        *dtaara
054000100621     c                   ENDIF
054100961003     C*  Richiamo XPARUT
054200961003     C                   Z-ADD     1             CODUT
054300961003     C                   CALL      'X§PARUT'
054400961003     C                   PARM                    UT§DSE
054500961003     C                   MOVEL     REC80         CNCR80
054600961003     C*  Ricerca capoconti
054700961003     C                   DO        50            X
054800961003     C                   MOVE      TCU(X)        TCUDS
054900961003     C     F56           IFEQ      'CG'
055000961003     C     F34           ANDEQ     '01'
055100961003     C                   Z-ADD     KCU(X)        KCI               4 0
055200961003     C                   END
055300961003     C                   END
055400961003     C                   MOVEL     RAGUT         RSUT
055500961003     C*  Chiavi di accesso
055600961003     C     KACO          KLIST
055700961003     C                   KFLD                    KKUT
055800961003     C                   KFLD                    KKCC
055900961003     C                   KFLD                    KKSC
056000020702     C*    KTAB1         KLIST
056100020702     C*                  KFLD                    KKUT
056200020702     C*                  KFLD                    KCOD1
056300020702     C*    KTAB2         KLIST
056400020702     C*                  KFLD                    KKUT
056500020702     C*                  KFLD                    KCOD1
056600020702     C*                  KFLD                    KKEY
056700961008     C     KBRV          KLIST
056800961008     C                   KFLD                    KCCM
056900961008     C                   KFLD                    KNFE
057000980326     C     KBRV1         KLIST
057100980326     C                   KFLD                    KLNP
057200980326     C                   KFLD                    KNFE
057300961008     C     KFVV          KLIST
057400961008     C                   KFLD                    KNPG
057500961008     C                   KFLD                    KNFV
057600961008     C                   KFLD                    KFGS
057700100621      *
057800100621     C* DEFINIZIONE CHIAVI DI ACCESSO
057900100621     C     Kwalc         KLIST
058000100621     C                   KFLD                    WalPGM
058100100621     C                   KFLD                    WalNOJ
058200100621     C                   KFLD                    WalNUS
058300100621     C                   KFLD                    WalNRJ
058400100621      *
058500961003     C*  Definizione variabili
058600961003     C     *LIKE         DEFINE    ACOKUT        KKUT
058700961003     C     *LIKE         DEFINE    ACOKCC        KKCC
058800961003     C     *LIKE         DEFINE    ACOKSC        KKSC
058900961003     C     *LIKE         DEFINE    TABCOD        KCOD
059000961008     C     *LIKE         DEFINE    BRVCCM        KCCM
059100980326     C     *LIKE         DEFINE    BRVLNP        KLNP
059200961008     C     *LIKE         DEFINE    BRVNFE        KNFE
059300961008     C     *LIKE         DEFINE    FVVNPG        KNPG
059400961008     C     *LIKE         DEFINE    FVVNFV        KNFV
059500961008     C     *LIKE         DEFINE    FVVFGS        KFGS
059600020702     C*    *LIKE         DEFINE    TBLCOD        KCOD1
059700020702     C*    *LIKE         DEFINE    TBLKEY        KKEY
059800961009     C     *LIKE         DEFINE    V1CFGS        WSVFGS
059900961008     C*  Reperisco filiale gestione in base al profilo utente
060000961008     C                   SETOFF                                       01
060100020506     c     simtpp        ifeq      '2'
060200020506     c     simtpp        oreq      *blanks
060300020506     c                   movel     simpou        v1cfgs
060400961008     C                   ELSE
060500961008     C                   SETON                                        01
060600961008     C                   Z-ADD     SIMFEL        V1CFGS
060700961008     C                   END
060800961009     C                   Z-ADD     V1CFGS        WSVFGS
060900020916     C*  Carico £1
061000020916     c                   clear                   trul06ds
061100020916     c                   eval      d06cod = '£1'
061200020916     c                   movel     simfel        d06key
061300020916     c                   movel     trul06ds      kpjbu
061400020916     c                   call      'TRUL06R'
061500020916     c                   parm                    kpjba
061600020916     c                   movel     kpjbu         trul06ds
061700020916     c                   movea     lin           £1
061800020916     C                   Z-ADD     1             KKUT
061900020916      *
062000961003     C*  Carico in schiera codici tabella - PT -
062100020702     C                   Z-ADD     0             X                 3 0
062200961003     C                   Z-ADD     0             KSC
062300961003     C                   MOVEL     'PT'          KCOD
062400961003     C     KCOD          CHAIN     EDTAB01L                           31
062500961003     C     *IN31         DOWEQ     '0'
062600961003     C     TABFLG        IFEQ      ' '
062700020916     C                   MOVEL     TABUNI        DSPT
062800020916      * solo se compresa nelle linee di partenza del terminal
062900130301     c**** §ptlnp        lookup    £1                                     32
063000130301     c***********        if        *in32 = *on
063100961003     C                   ADD       1             X
063200961011     C                   MOVEL     TABKEY        CPT(X)
063300961003     C                   Z-ADD     §PTKSC        KSC(X)
063400980320     C                   MOVEL     §PTLNP        LNP(X)
063500130301     c***********        end
063600961003     C                   END
063700961003     C     KCOD          READE     EDTAB01L                               31
063800961003     C                   END
063900961008     C*  Carico continuazione
064000961008     C                   MOVEA     *BLANKS       DPU
064100961008     C                   MOVEL     'PU'          TABCOD
064200961008     C     TABCOD        CHAIN     EDTAB01L                           30
064300961008     C     *IN30         DOWEQ     '0'
064400961008     C     TABFLG        IFEQ      ' '
064500961008     C                   MOVEL     TABKEY        CODPU            35
064600961008     C                   Z-ADD     1             X
064700961008     C     CODPU         LOOKUP    CPT(X)                                 32
064800961008     C     *IN32         IFEQ      '1'
064900961008     C                   MOVEL     TABUNI        DPU(X)
065000980324     C                   MOVEL     TABUNI        DSPU
065100980320     C     §PUTPC        IFNE      'P'
065200980320     C                   Z-ADD     0             LNP(X)
065300980320     C                   ENDIF
065400030715     C****               ELSE
065500030715     C****               Z-ADD     0             LNP(X)
065600980320     C                   ENDIF
065700980324     C                   ENDIF
065800961008     C     TABCOD        READE     EDTAB01L                               30
065900961008     C                   END
066000020726     C*  Carico in schiera codici collegati - CL -
066100020726     C                   Z-ADD     0             X                 3 0
066200020726     C                   Z-ADD     0             KCL
066300020726     C                   Z-ADD     0             KPT
066400020726     C                   MOVEL     'CL'          KCOD
066500020726     C     KCOD          CHAIN     EDTAB01L                           31
066600020726     C     *IN31         DOWEQ     '0'
066700020726     C     TABFLG        IFEQ      ' '
066800020726     C                   ADD       1             X
066900020726     C                   MOVEL     TABUNI        DSCL
067000020726     C                   MOVEL     TABKEY        KPT(X)
067100020726     C                   Z-ADD     §CLKSC        KCL(X)
067200020726     C                   END
067300020726     C     KCOD          READE     EDTAB01L                               31
067400020726     C                   END
067500961003     C*  Inizializzo variabili
067600961003     C                   MOVEL     'N'           WFINE             1
067700100621      ***
067800100621     C                   TIME                    W0140            14 0
067900100621     C                   MOVEl     W0140         Wora              6 0
068000100621     C                   MOVE      W0140         WDAT              8 0
068100100621     C*
068200100621     C                   Z-ADD     Wdat          G02DAT
068300100621     C                   MOVEL     *BLANK        G02ERR
068400100621     C                   CALL      'XSRDA8'
068500100621     C                   PARM                    WLBDA8
068600100621     C* UDATE A 8 IN AAAA/MM/GG
068700100621     C                   move      G02INV        dataoggi
068800100621     C*
068900100621     C* scrive il record di allocazione x trasmissione
069000100621     C                   clear                   Fiwalc00
069100100621     C                   movel     w0140         walOrc
069200100621     C                   movel     g02inv        waldac
069300100621     C                   eval      WalSIF = KNSIF
069400100621     C                   eval      WalPGM = 'TRTC87R   '
069500100621     C                   eval      WalNOJ = KNMEB
069600100621     C                   eval      WalNUS = KNMUS
069700100621     C                   eval      WalNRJ = KNRJO
069800100621     C                   eval      WalFLD = 'FILIALE   ' + %editc(UTEFIL:'X')
069900100622     c                                      + ' - ATR'
070000100621     C                   write     Fiwalc00
070100100621     c                   feod      Fiwalc1L
070200100621     C***
070300961003     C*
070400961003     C                   ENDSR
070500980320**
070600961008Punto operativo inesistente o annullato                                01
070700961008Punto operativo non gestito su questo AS                               02
070800961008Il punto operativo non si gestisce in modo autonomo                    03
070900961008Codice cliente inesistente o annullato                                 04
071000961008Codice cliente non relativo ad un partner                              05
071100980401Per cliente/Lnp selezionato non ci sono date di arrivo da trasmettere  06
071200980401Per cliente/Lnp selezionato non devo trasmettere le date di arrivo     07
071300980320Selezionare un codice cliente o un linea di partenza di un partner     08
071400980320Impossibile selezionare entrambi (sia lnp che codice cliente)          09
071500980320La Lnp non esiste su tabella EDI o non appartiene ad un partner        10
