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
003900150415     D LNPT            S              3  0 DIM(999)                             COD lnp partner
004000110712     D KCL             S              7  0 DIM(999)                             ns.codice
004100150415     D KLP             S              3  0 DIM(999)                             ns.codice
004200110712     D KPT             S              7  0 DIM(999)                             ns.codice
004300961003     D* Schiera errori
004400980324     D ERR             S             70    DIM(10) CTDATA PERRCD(1)
004500961003     D*---------------------------------------------------------------------*
004600961003     D* DS
004700961003     D*---------------------------------------------------------------------*
004800961003     D KPJBA         E DS
004900100621     d   KPJBUS        s                   like(KPJBU)
005000961003     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005100961003     D  TCU                  398    697
005200961003     D                                     DIM(50)                              Flg 8 tp.conto
005300961003     D  KCU                  698    847P 0
005400961003     D                                     DIM(50)                              Capiconto
005500961003     D                                     PACKEVEN
005600961003     D CNCR80        E DS
005700020702     d trul06ds      e ds
005800020702     d  lin                    1     90  0 dim(30)
005900961003     D* Ds scomposzione tipo capoconti
006000961003     D TCUDS           DS
006100961003     D  F34                    3      4
006200961003     D  F56                    5      6
006300961003     D DSPT          E DS                  EXTNAME(EDIDSPT)
006400961007     D DSPU          E DS                  EXTNAME(EDIDSPU)
006500020726     D DSCL          E DS                  EXTNAME(EDIDSCL)
006600100621      *
006700100621     D ddatiute      e ds                  prefix(UT_)
006800100621     D azuteds       e ds                  extname(AZUTE00F)
006900100621     D tibs34ds      E DS                  inz
007000100621     d dataoggi        s               d   datfmt(*iso)
007100100621      *---------------------------------------------------------------------*
007200100621     D TRTC87ds0     E DS
007300961007     D TRTC87        E DS                  EXTNAME(TRTC87DS)
007400100621      *---------------------------------------------------------------------*
007500961003     D*  Ds per inversione data
007600961003     D WLBDA8          DS
007700961003     D  G02DAT                 1      8  0
007800961003     D  G02INV                 9     16  0
007900961003     D  G02ERR                17     17
008000961003     D  G02TGI                18     22  0
008100961008     D*  Ds per reperimento filiale da secondi livelli
008200020506     D*                DS
008300020506     D* WTPPRF                 1      3
008400020506     D* WFGS                   4      6
008500020506     D* WSEI                   1      6
008600961003     D                SDS
008700961003     D  NOMPGM                 1     10
008800961003      *----------------------------------------------------------------
008900961003      *  FILE
009000961003      *----------------------------------------------------------------
009100961003     IAZORG01L  AA
009200961008     I                             P    4    5 0ORGFIL
009300961008     I                                  6    6  ORGFVA
009400961003     I                                 14   33  ORGDES
009500961003     C*---------------------------------------------------------------------*
009600961003     C* Ciclo principale
009700961003     C*---------------------------------------------------------------------*
009800100621      *
009900100621     C                   clear                   trtc87ds0
010000100621     c                   eval      FN0PGM  = 'TRTC87R'
010100100621     c                   eval      FN0TIPO = 'FILIALE'
010200100621     c                   eval      FN0CODICE = %editc(UTEFIL:'X')
010300100621     C                   MOVEL     KPJBU         kpjbus
010400100621     C                   clear                   kpjbu
010500100621     C                   MOVEL     trtc87ds0     kpjbu
010600100621     c                   call      'TRTC87R0'
010700100621     c                   parm                    kpjba
010800100621     C                   MOVEL     kpjbu         TRTC87ds0
010900100621     C*   se già utilizzata da altro utente della stessa filiale fa uscire
011000100621     C*   forzatamente.
011100100621     c                   if        Fn0FLG <> *blank
011200100621     c                   goto      FINE
011300100621     c                   end
011400100621     C*
011500100621     C                   MOVEL     KPJBUs        kpjbu
011600100621     C*
011700100621      *
011800961003     C* Inizializzo prima videata
011900961003     C                   EXSR      INZ01
012000961003      * Loop gestione videate
012100961003     C     WFINE         DOWEQ     'N'
012200961008     C     WTPVID        CASEQ     '1'           GESD01                         Sce.cliente
012300961008     C     WTPVID        CASEQ     '2'           GESS02                         Subfile
012400961003     C                   END
012500961003     C                   END
012600961003      * Fine Lavoro
012700961003     C     FINE          TAG
012800100621     C*
012900100621     C*  deve cancellare il record di allocazione x impedire le trasmissioni
013000100621     C*   in sede durante l'utilizzo di questo programma.
013100100621     C     Kwalc         setll     fiwalc1l
013200100621     C     Kwalc         reade     fiwalc1l
013300100621     c                   dow       not %eof(fiwalc1l)
013400100621     c                   delete    fiwalc00
013500100621     C     Kwalc         reade     fiwalc1l
013600100621     c                   end
013700100621      *
013800100621     c                   commit
013900100621      *
014000961003     C                   SETON                                        LR
014100961003     C*---------------------------------------------------------------*
014200961003     C*  INZ01: Inizializzo prima videata
014300961003     C*---------------------------------------------------------------*
014400961003     C     INZ01         BEGSR
014500961003     C*
014600980320     C                   SETOFF                                       404142
014700980320     C                   SETOFF                                       1828
014800961003     C                   MOVEL     *BLANKS       $MSG
014900961003     C                   Z-ADD     0             V1CCCM
015000980320     C                   Z-ADD     0             V1CLNP
015100961009     C                   MOVEL     '1'           WTPVID            1
015200961003     C                   MOVEL     NOMPGM        V1CPGM
015300961003     C                   MOVEL     *BLANKS       V1DCCM
015400980320     C                   MOVEL     *BLANKS       V1DLNP
015500961003     C                   MOVEL     *BLANKS       V1DFGS
015600961003     C*  Decodifico filiale in gestione
015700961008     C                   MOVEL     *BLANKS       ORGDES
015800961003     C                   Z-ADD     V1CFGS        ORGFIL
015900961003     C     ORGFIL        CHAIN     AZORG01L                           31
016000961003     C  N31              MOVEL     ORGDES        V1DFGS
016100961003     C*
016200961003     C                   ENDSR
016300961003     C*---------------------------------------------------------------*
016400961003     C*  INZ02: Inizializzo seconda videata
016500961003     C*---------------------------------------------------------------*
016600961003     C     INZ02         BEGSR
016700961003     C*
016800961003     C                   SETOFF                                       28
016900961003     C                   MOVEL     *BLANKS       $MSG
017000961003     C                   MOVEL     NOMPGM        V2CPGM
017100961008     C                   MOVEL     '2'           WTPVID
017200961008     C                   Z-ADD     V1CCCM        V2CCCM
017300961008     C                   MOVEL     V1DCCM        V2DCCM
017400980401     C                   Z-ADD     V1CLNP        V2CLNP
017500980401     C                   MOVEL     V1DLNP        V2DLNP
017600961011     C                   MOVEL     V1CFGS        V2CFGS
017700961003     C*  Pulisco subfile
017800961009     C                   Z-ADD     0             NRR2              4 0
017900961003     C                   SETOFF                                       2021
018000961003     C                   WRITE     TC87C02
018100961003     C                   SETON                                        2021
018200980326     C*  se ho scelto di lavorare per lnp accendo 06  altrimenti 05
018300980326     C     V1CLNP        COMP      0                                  060605
018400961007      *  Eseguo caricamento in base al cod.cliente prima videata
018500961008     C                   Z-ADD     0             NRR2
018600961008      *  Prima lettura:
018700980326     C     V1CLNP        IFEQ      0
018800961008     C     KCCM          CHAIN     EDBRV01L                           30
018900980326     C                   ELSE
019000980326     C     KLNP          CHAIN     EDBRV04L                           30
019100980326     C                   END
019200961008     C     *IN30         DOWEQ     '0'                                          Eof EDBRV or
019300961008     C                   EXSR      ROLLUP
019400961008     C                   END
019500961003     C*
019600961003     C                   ENDSR
019700961003     C*---------------------------------------------------------------*
019800961003     C*  GESD01: Gestione prima videata
019900961003     C*---------------------------------------------------------------*
020000961003     C     GESD01        BEGSR
020100961003     C*
020200961008     C                   EXFMT     TC87D01
020300961008     C                   SETOFF                                       284041
020400961003     C*  Fine Lavoro
020500961003     C     *INKC         IFEQ      '1'
020600961003     C                   MOVEL     'S'           WFINE
020700961003     C                   GOTO      FINVD1
020800961003     C                   END
020900961008     C*  Cambio filiale
021000961008     C     *INKS         IFEQ      '1'
021100961008     C                   SETON                                        18
021200961008     C                   GOTO      FINVD1
021300961008     C                   END
021400961003     C*  Controlli
021500961003     C                   EXSR      CTR01
021600961007     C   28              GOTO      FINVD1
021700961007     C*  Caricamento subfile
021800961007     C                   EXSR      INZ02
021900961003     C*
022000961003     C     FINVD1        ENDSR
022100961003     C*---------------------------------------------------------------*
022200961003     C*  GESS02: Gestione subfile pieno
022300961003     C*---------------------------------------------------------------*
022400961003     C     GESS02        BEGSR
022500961003     C*
022600961009     C                   WRITE     TC87T02
022700961009     C                   EXFMT     TC87C02
022800961003     C*  Fine Lavoro
022900961003     C     *INKC         IFEQ      '1'
023000961003     C                   MOVEL     'S'           WFINE
023100961003     C                   GOTO      FINVS2
023200961003     C                   END
023300961003     C*  Ritorno
023400961003     C     *INKL         IFEQ      '1'
023500961003     C                   EXSR      INZ01
023600961003     C                   GOTO      FINVS2
023700961003     C                   END
023800961003     C*  Controlli
023900961003     C                   EXSR      CTR02
024000961003     C*
024100961003     C     FINVS2        ENDSR
024200961003     C*---------------------------------------------------------------*
024300961003     C*  CTR01: Controlli prima videata
024400961003     C*---------------------------------------------------------------*
024500961003     C     CTR01         BEGSR
024600961003     C*
024700961008     C*  Filiale in gestione: controllo se esiste
024800961008     C     V1CFGS        IFNE      WSVFGS
024900961009     C                   Z-ADD     V1CFGS        ORGFIL
025000961008     C     ORGFIL        CHAIN     AZORG01L                           30
025100961008     C     *IN30         IFEQ      '1'
025200961011     C     ORGFVA        ORNE      *BLANK
025300961008     C                   SETON                                        4028
025400961008     C                   MOVEL     ERR(1)        $MSG
025500961011     C                   GOTO      FINCT1
025600961008     C                   END
025700961008     C                   MOVEL     ORGDES        V1DFGS
025800961008     C*  ... controllo se gestita su questo as
025900961008     C     V1CFGS        LOOKUP    £1                                     32
026000961008     C     *IN32         IFEQ      '0'
026100961008     C                   SETON                                        4028
026200961008     C                   MOVEL     ERR(2)        $MSG
026300961011     C                   GOTO      FINCT1
026400961008     C                   END
026500980113     C                   END
026600980320     C*  Devo selezionare o la linea di partenza di un partner o un
026700980320     C*  codice cliente
026800980320     C     V1CCCM        IFEQ      0
026900980320     C     V1CLNP        ANDEQ     0
027000980320     C                   SETON                                        424128
027100980320     C                   MOVEL     ERR(8)        $MSG
027200980320     C                   GOTO      FINCT1
027300980320     C                   END
027400980320     C* ... ma non entrambi
027500980320     C     V1CCCM        IFNE      0
027600980320     C     V1CLNP        ANDNE     0
027700980320     C                   SETON                                        424128
027800980320     C                   MOVEL     ERR(9)        $MSG
027900980320     C                   GOTO      FINCT1
028000980320     C                   END
028100980320     C*  Controllo se cliente esiste su CNACO
028200980320     C     V1CCCM        IFGT      0
028300961003     C                   Z-ADD     1             KKUT
028400961003     C                   Z-ADD     KCI           KKCC
028500961003     C                   Z-ADD     V1CCCM        KKSC
028600961008     C                   MOVEL     *BLANKS       ACORAG
028700961003     C     KACO          CHAIN     CNACO00F                           30
028800961003     C     *IN30         IFEQ      '1'
028900961003     C     ACOFLG        ORNE      ' '
029000961008     C                   SETON                                        2841
029100961008     C                   MOVEL     ERR(4)        $MSG
029200961003     C                   GOTO      FINCT1
029300961003     C                   END
029400130301     C*-----****
029500130301      ** occorre controllare che la filiale del codice cliente appartenga
029600130301      **  al terminal selezionato
029700130301     c                   movel     v1cccm        Filiale_CLI       3 0
029800130301     C     Filiale_CLI   LOOKUP    £1                                     32
029900130301     C     *IN32         IFEQ      '0'
030000130301      **
030100130301      **  controlla sulla CL se esiste un legame e se c'è controlla con quello
030200130301     C                   Z-ADD     1             XX
030300130301     C     V1CCCM        LOOKUP    KCL(XX)                                32
030400150415     C                   clear                   Salva_LNP_CL      3 0
030500150415     C   32              z-add     KLP(XX)       Salva_LNP_CL
030600130301      **
030700130301     C   32              movel     KPT(XX)       Filiale_CLI
030800130301     C   32Filiale_CLI   LOOKUP    £1                                     32
030900150415      **---
031000150415      *   il controllo sui primi tre caratteri del codice cliente non funziona
031100150415      *   se il cliente è stato spostato su un'altra filiale mantenendo il vecchio
031200150415      *   codice.  Occorre verificare sulla tab.CL o PT la LNP legata al cliente/ptn
031300150415      **---
031400150415     C     *IN32         IFEQ      '0'
031500150415     c                   if        Salva_LNP_CL > 0
031600150415     C     Salva_LNP_CL  LOOKUP    £1                                     32
031700150415     c                   end
031800150415      * se non fosse sulla CL deve tentare anche sulla PT e poi agganciare con la LNP
031900150415     C     *IN32         IFEQ      '0'
032000150415     C                   Z-ADD     1             X
032100150415     C     V1CCCM        LOOKUP    KSC(X)                                 32
032200150415     C   32LNPT(X)       LOOKUP    £1                                     32
032300150415     C                   END
032400150415     C                   END
032500150415      **---
032600130301     C     *IN32         IFEQ      '0'
032700130301     C                   SETON                                          4128
032800130301     C                   MOVEL     ERR(2)        $MSG
032900130301     C                   GOTO      FINCT1
033000130301     C                   END
033100130301      **
033200130301     C                   END
033300130301     C*-----****
033400961104     C                   MOVE      V1CCCM        WCCN              7 0
033500961009     C                   Z-ADD     1             X
033600961008     C                   MOVEL     ACORAG        V1DCCM
033700961009     C     V1CCCM        LOOKUP    KSC(X)                                 32
033800020726      *
033900020726      *  Se non era presente sulla PT prova anche sull CL altrimenti segnala
034000020726      *   l'errore.
034100020726     C     *IN32         IFEQ      '0'
034200020726     C                   Z-ADD     1             XX                3 0
034300020726     C     V1CCCM        LOOKUP    KCL(XX)                                32
034400020726     C   32              Z-ADD     1             X
034500020726     C   32KPT(XX)       LOOKUP    KSC(X)                                 32
034600020726     C                   END
034700020221      *
034800961029     C*  NO Codice tabella PT
034900961029     C     *IN32         IFEQ      '0'
035000961008     C                   SETON                                        2841
035100961008     C                   MOVEL     ERR(5)        $MSG
035200961003     C                   GOTO      FINCT1
035300961003     C                   END
035400961009     C*  Controllo se devo trasmettere le date arrivo al cliente
035500961009     C                   MOVEL     DPU(X)        DSPU
035600961009     C     §PUDTA        IFEQ      ' '
035700961009     C                   SETON                                        2841
035800961009     C                   MOVEL     ERR(7)        $MSG
035900961009     C                   GOTO      FINCT1
036000961009     C                   END
036100961008     C*  Controllo se ha date di arrivo da spedire
036200961008     C                   Z-ADD     V1CCCM        KCCM
036300961008     C     KCCM          CHAIN     EDBRV01L                           30
036400961029     C     *IN30         IFEQ      '1'
036500961008     C                   SETON                                        2841
036600961008     C                   MOVEL     ERR(6)        $MSG
036700961008     C                   GOTO      FINCT1
036800961003     C                   END
036900130301      **
037000980326     C                   ELSE
037100130301      **
037200980326     C                   Z-ADD     V1CLNP        ORGFIL
037300980401     C                   MOVEL     *BLANKS       V1DLNP
037400980326     C     ORGFIL        CHAIN     AZORG01L                           30
037500980326     C     *IN30         IFEQ      '1'
037600980326     C     ORGFVA        ORNE      *BLANK
037700980326     C                   SETON                                        2842
037800980326     C                   MOVEL     ERR(01)       $MSG
037900980326     C                   GOTO      FINCT1
038000980401     C*  Decodifico LNP
038100980401     C                   ELSE
038200130301     C*-----****
038300130301     c                   movel     v1clnp        Filiale_CLI       3 0
038400130301     C     Filiale_CLI   LOOKUP    £1                                     32
038500130301     C     *IN32         IFEQ      '0'
038600130301     C                   SETON                                        42  28
038700130301     C                   MOVEL     ERR(2)        $MSG
038800130301     C                   GOTO      FINCT1
038900130301     C                   END
039000130301     C*-----****
039100980401     C                   MOVEL     ORGDES        V1DLNP
039200980326    2C                   ENDIF
039300980326     C*  se richiesta elaborazione x lnp ctrl se è un partner
039400980326     C                   Z-ADD     1             YY                3 0
039500980326     C     V1CLNP        LOOKUP    LNP(YY)                                30
039600980326    2C     *IN30         IFEQ      *OFF
039700980326     C                   SETON                                        2842
039800980326     C                   MOVEL     ERR(10)       $MSG
039900980326     C                   GOTO      FINCT1
040000980326    2C                   ENDIF
040100980326     C*  Controllo se devo trasmettere le date arrivo
040200980401     C                   MOVEL     DPU(YY)       DSPU
040300980326     C     §PUDTA        IFEQ      ' '
040400980326     C                   SETON                                        2842
040500980326     C                   MOVEL     ERR(7)        $MSG
040600980326     C                   GOTO      FINCT1
040700980326     C                   END
040800980326     C*  Controllo se ha date di arrivo da spedire
040900980326     C                   Z-ADD     V1CLNP        KLNP
041000980326     C     KLNP          CHAIN     EDBRV04L                           30
041100980326     C     *IN30         IFEQ      '1'
041200980326     C                   SETON                                        2842
041300980326     C                   MOVEL     ERR(6)        $MSG
041400980326     C                   GOTO      FINCT1
041500980326     C                   END
041600980326     C*
041700980320     C                   END
041800961003     C*
041900961003     C     FINCT1        ENDSR
042000961003     C*---------------------------------------------------------------*
042100961003     C*  CTR02: Controlli seconda videata
042200961003     C*---------------------------------------------------------------*
042300961003     C     CTR02         BEGSR
042400961003     C*
042500970228     C                   MOVEL     'N'           WCANC
042600961011     C                   CLEAR                   TRTC87
042700961003     C*  Controllo selezioni effettuate
042800961008     C                   READC     TC87S02                                29
042900961003     C     *IN29         DOWEQ     '0'
043000961011     C     D87CMD        ANDNE     '03'
043100970228     C     V2CSCE        IFNE      ' '
043200961003     C*  Richiamo pgm per esecuzione scrittura VAB
043300961008     C                   CLEAR                   TRTC87
043400980401     C                   MOVEL     §PUBS1        D87BS1
043500980409     C                   MOVEL     §PUECC        D87SEC
043600961008     C                   Z-ADD     V2CNFE        D87NFE
043700961008     C                   MOVEL     V2CDFE        G02DAT
043800961008     C                   Z-ADD     0             G02INV
043900961008     C                   MOVEL     *BLANKS       G02ERR
044000980320     C                   CALL      'XSRDA8'
044100961008     C                   PARM                    WLBDA8
044200961008     C                   Z-ADD     G02INV        D87DFE
044300961008     C                   Z-ADD     V2CCCM        D87CCM
044400980320     C                   Z-ADD     V2CLNP        D87LNP
044500961104     C                   Z-ADD     WCCN          D87CCN
044600970228     C     V2CSCE        IFEQ      '1'
044700961009     C                   MOVEL     V2DCCM        D87DEC
044800980401     C                   MOVEL     V2DLNP        D87DLP
044900961009     C                   MOVEL     V2CERR        D87ERR
045000961024     C                   MOVEL     'S'           D87WRT
045100980320     C                   MOVEL     V2CRIT        D87RIT
045200961008     C                   MOVEL     TRTC87        KPJBU
045300961011      *  Se ho avuto errori in precedenti elab. sfleggo dati
045400961011     C                   CALL      'TRTC87R5'
045500961011     C                   PARM                    KPJBA
045600961008      *  Richiamo pgm x per aggiornamento archivio errori in
045700961008      *  ricezione bolle
045800961008     C                   CALL      'TRTC87R1'
045900961008     C                   PARM                    KPJBA
046000961008      *  Richiamo pgm gestione videata errori su foglio entrata
046100961008     C                   CALL      'TRTC87R2'
046200961008     C                   PARM                    KPJBA
046300961008     C                   MOVEL     KPJBU         TRTC87
046400970228     C                   END
046500970228     C* Call a pgm che cancella foglio entrata
046600970305     C   KQV2CSCE        IFEQ      '4'
046700970228     C                   MOVEL     TRTC87        KPJBU
046800970228     C                   CALL      'TRTC87R4'
046900970228     C                   PARM                    KPJBA
047000970228     C                   MOVEL     'S'           WCANC             1
047100970228     C                   END
047200961008      *  Aggiorno record subfile
047300961008     C                   MOVE      V2CIN2        *IN02
047400961008     C                   MOVEL     *BLANKS       V2CSCE
047500961008     C                   UPDATE    TC87S02
047600970228     C                   END
047700961008     C*  Lettura successiva subfile
047800961008     C                   READC     TC87S02                                29
047900961003     C                   END
048000961011     C*
048100961011      *  F12=Sfleggo record annullati e riemetto videata
048200961011     C     D87CMD        IFEQ      '12'
048300961011     C                   CALL      'TRTC87R5'
048400961011     C                   PARM                    KPJBA
048500961011     C                   EXSR      INZ01
048600961011     C                   END
048700961011     C*
048800961011     C*  F6=Riemetto prima videata
048900961011     C     D87CMD        IFEQ      '06'
049000970228     C     WCANC         OREQ      'S'
049100961008     C                   EXSR      INZ01
049200961008     C                   END
049300961011     C*
049400961011     C     D87CMD        IFEQ      '03'
049500961011      *  F3=Sfleggo record annullati
049600961011     C                   CALL      'TRTC87R5'
049700961011     C                   PARM                    KPJBA
049800961011     C                   MOVEL     'S'           WFINE
049900961011     C                   END
050000961003     C*
050100961003     C                   ENDSR
050200961003     C*----------------------------------------------------*
050300961003     C*   ROLLUP: Caricamento dati da EDVAB                *
050400961003     C*----------------------------------------------------*
050500961003     C     ROLLUP        BEGSR
050600961003     C*
050700961008     C                   CLEAR                   TC87S02
050800961008     C                   MOVEL     *BLANKS       V2CSCE
050900961008      *  Eseguo posizionamento su foglio entrata
051000961008     C                   MOVEL     '5'           KNPG
051100961008     C                   Z-ADD     BRVNFE        KNFV
051200961008     C                   Z-ADD     V1CFGS        KFGS
051300961008     C     KFVV          CHAIN     FNFVV01L                           31
051400961008     C                   Z-ADD     FVVNFV        V2CNFE
051500961008     C                   MOVEL     FVVDSF        V2DNFE
051600961008     C                   Z-ADD     FVVDFV        G02INV
051700961008     C                   Z-ADD     0             G02DAT
051800961008     C                   MOVEL     '3'           G02ERR
051900961008     C                   CALL      'XSRDA8'
052000961008     C                   PARM                    WLBDA8
052100961008     C                   Z-ADD     G02DAT        V2CDFE
052200961008     C                   MOVEL     'S'           V2CERR
052300980320     C                   MOVEL     ' '           V2CRIT
052400961008      *  Se scrivo EDBRV con i soli errori (flag tab.PT) non
052500961008      *  permetto che mi selezionino tutto
052600961008     C     §PUDTB        IFEQ      'E'
052700961008     C                   SETOFF                                       02
052800961008     C                   ELSE
052900961008     C                   SETON                                        02
053000961008     C                   END
053100961008     C                   MOVEL     *IN02         V2CIN2
053200961008     C                   ADD       1             NRR2
053300961008     C                   WRITE     TC87S02
053400980326     C*  Mi posiziono sul prossimo Foglio Entrata
053500961008     C                   MOVEL     BRVNFE        KNFE
053600980326     C     V1CLNP        IFEQ      0
053700961008     C     KBRV          SETGT     EDBRV01L
053800961008     C     KCCM          READE     EDBRV01L                               30
053900980326     C                   ELSE
054000980326     C     KBRV1         SETGT     EDBRV04L
054100980326     C     KLNP          READE     EDBRV04L                               30
054200980326     C                   END
054300961003     C*
054400961003     C                   ENDSR
054500961003     C*----------------------------------------------------*
054600961003     C*   *INZSR: Operazioni iniziali                      *
054700961003     C*----------------------------------------------------*
054800961010     C     *INZSR        BEGSR
054900961003     C*
055000961003     C     *ENTRY        PLIST
055100961003     C                   PARM                    KPJBA
055200100621      *
055300100621     c     *dtaara       define    §azute        azuteds
055400100621     c     *dtaara       define    §datiute      ddatiute
055500100621     C                   in(E)     *dtaara
055600100621     C                   IF        %Error  or  RSUT = *blanks
055700100621     C                   call      'TIBS34R'
055800100621     C                   parm                    Tibs34Ds
055900100621     C                   in        *dtaara
056000100621     c                   ENDIF
056100961003     C*  Richiamo XPARUT
056200961003     C                   Z-ADD     1             CODUT
056300961003     C                   CALL      'X§PARUT'
056400961003     C                   PARM                    UT§DSE
056500961003     C                   MOVEL     REC80         CNCR80
056600961003     C*  Ricerca capoconti
056700961003     C                   DO        50            X
056800961003     C                   MOVE      TCU(X)        TCUDS
056900961003     C     F56           IFEQ      'CG'
057000961003     C     F34           ANDEQ     '01'
057100961003     C                   Z-ADD     KCU(X)        KCI               4 0
057200961003     C                   END
057300961003     C                   END
057400961003     C                   MOVEL     RAGUT         RSUT
057500961003     C*  Chiavi di accesso
057600961003     C     KACO          KLIST
057700961003     C                   KFLD                    KKUT
057800961003     C                   KFLD                    KKCC
057900961003     C                   KFLD                    KKSC
058000020702     C*    KTAB1         KLIST
058100020702     C*                  KFLD                    KKUT
058200020702     C*                  KFLD                    KCOD1
058300020702     C*    KTAB2         KLIST
058400020702     C*                  KFLD                    KKUT
058500020702     C*                  KFLD                    KCOD1
058600020702     C*                  KFLD                    KKEY
058700961008     C     KBRV          KLIST
058800961008     C                   KFLD                    KCCM
058900961008     C                   KFLD                    KNFE
059000980326     C     KBRV1         KLIST
059100980326     C                   KFLD                    KLNP
059200980326     C                   KFLD                    KNFE
059300961008     C     KFVV          KLIST
059400961008     C                   KFLD                    KNPG
059500961008     C                   KFLD                    KNFV
059600961008     C                   KFLD                    KFGS
059700100621      *
059800100621     C* DEFINIZIONE CHIAVI DI ACCESSO
059900100621     C     Kwalc         KLIST
060000100621     C                   KFLD                    WalPGM
060100100621     C                   KFLD                    WalNOJ
060200100621     C                   KFLD                    WalNUS
060300100621     C                   KFLD                    WalNRJ
060400100621      *
060500961003     C*  Definizione variabili
060600961003     C     *LIKE         DEFINE    ACOKUT        KKUT
060700961003     C     *LIKE         DEFINE    ACOKCC        KKCC
060800961003     C     *LIKE         DEFINE    ACOKSC        KKSC
060900961003     C     *LIKE         DEFINE    TABCOD        KCOD
061000961008     C     *LIKE         DEFINE    BRVCCM        KCCM
061100980326     C     *LIKE         DEFINE    BRVLNP        KLNP
061200961008     C     *LIKE         DEFINE    BRVNFE        KNFE
061300961008     C     *LIKE         DEFINE    FVVNPG        KNPG
061400961008     C     *LIKE         DEFINE    FVVNFV        KNFV
061500961008     C     *LIKE         DEFINE    FVVFGS        KFGS
061600020702     C*    *LIKE         DEFINE    TBLCOD        KCOD1
061700020702     C*    *LIKE         DEFINE    TBLKEY        KKEY
061800961009     C     *LIKE         DEFINE    V1CFGS        WSVFGS
061900961008     C*  Reperisco filiale gestione in base al profilo utente
062000961008     C                   SETOFF                                       01
062100020506     c     simtpp        ifeq      '2'
062200020506     c     simtpp        oreq      *blanks
062300020506     c                   movel     simpou        v1cfgs
062400961008     C                   ELSE
062500961008     C                   SETON                                        01
062600961008     C                   Z-ADD     SIMFEL        V1CFGS
062700961008     C                   END
062800961009     C                   Z-ADD     V1CFGS        WSVFGS
062900020916     C*  Carico £1
063000020916     c                   clear                   trul06ds
063100020916     c                   eval      d06cod = '£1'
063200020916     c                   movel     simfel        d06key
063300020916     c                   movel     trul06ds      kpjbu
063400020916     c                   call      'TRUL06R'
063500020916     c                   parm                    kpjba
063600020916     c                   movel     kpjbu         trul06ds
063700020916     c                   movea     lin           £1
063800020916     C                   Z-ADD     1             KKUT
063900020916      *
064000961003     C*  Carico in schiera codici tabella - PT -
064100020702     C                   Z-ADD     0             X                 3 0
064200961003     C                   Z-ADD     0             KSC
064300961003     C                   MOVEL     'PT'          KCOD
064400961003     C     KCOD          CHAIN     EDTAB01L                           31
064500961003     C     *IN31         DOWEQ     '0'
064600961003     C     TABFLG        IFEQ      ' '
064700020916     C                   MOVEL     TABUNI        DSPT
064800020916      * solo se compresa nelle linee di partenza del terminal
064900130301     c**** §ptlnp        lookup    £1                                     32
065000130301     c***********        if        *in32 = *on
065100961003     C                   ADD       1             X
065200961011     C                   MOVEL     TABKEY        CPT(X)
065300961003     C                   Z-ADD     §PTKSC        KSC(X)
065400980320     C                   MOVEL     §PTLNP        LNP(X)
065500150415     C                   MOVEL     §PTLNP        LNPT(X)
065600130301     c***********        end
065700961003     C                   END
065800961003     C     KCOD          READE     EDTAB01L                               31
065900961003     C                   END
066000961008     C*  Carico continuazione
066100961008     C                   MOVEA     *BLANKS       DPU
066200961008     C                   MOVEL     'PU'          TABCOD
066300961008     C     TABCOD        CHAIN     EDTAB01L                           30
066400961008     C     *IN30         DOWEQ     '0'
066500961008     C     TABFLG        IFEQ      ' '
066600961008     C                   MOVEL     TABKEY        CODPU            35
066700961008     C                   Z-ADD     1             X
066800961008     C     CODPU         LOOKUP    CPT(X)                                 32
066900961008     C     *IN32         IFEQ      '1'
067000961008     C                   MOVEL     TABUNI        DPU(X)
067100980324     C                   MOVEL     TABUNI        DSPU
067200980320     C     §PUTPC        IFNE      'P'
067300980320     C                   Z-ADD     0             LNP(X)
067400980320     C                   ENDIF
067500030715     C****               ELSE
067600030715     C****               Z-ADD     0             LNP(X)
067700980320     C                   ENDIF
067800980324     C                   ENDIF
067900961008     C     TABCOD        READE     EDTAB01L                               30
068000961008     C                   END
068100020726     C*  Carico in schiera codici collegati - CL -
068200020726     C                   Z-ADD     0             X                 3 0
068300020726     C                   Z-ADD     0             KCL
068400020726     C                   Z-ADD     0             KPT
068500020726     C                   MOVEL     'CL'          KCOD
068600020726     C     KCOD          CHAIN     EDTAB01L                           31
068700020726     C     *IN31         DOWEQ     '0'
068800020726     C     TABFLG        IFEQ      ' '
068900020726     C                   ADD       1             X
069000020726     C                   MOVEL     TABUNI        DSCL
069100020726     C                   MOVEL     TABKEY        KPT(X)
069200020726     C                   Z-ADD     §CLKSC        KCL(X)
069300150415     C                   Z-ADD     §CLLNP        KLP(X)
069400020726     C                   END
069500020726     C     KCOD          READE     EDTAB01L                               31
069600020726     C                   END
069700961003     C*  Inizializzo variabili
069800961003     C                   MOVEL     'N'           WFINE             1
069900100621      ***
070000100621     C                   TIME                    W0140            14 0
070100100621     C                   MOVEl     W0140         Wora              6 0
070200100621     C                   MOVE      W0140         WDAT              8 0
070300100621     C*
070400100621     C                   Z-ADD     Wdat          G02DAT
070500100621     C                   MOVEL     *BLANK        G02ERR
070600100621     C                   CALL      'XSRDA8'
070700100621     C                   PARM                    WLBDA8
070800100621     C* UDATE A 8 IN AAAA/MM/GG
070900100621     C                   move      G02INV        dataoggi
071000100621     C*
071100100621     C* scrive il record di allocazione x trasmissione
071200100621     C                   clear                   Fiwalc00
071300100621     C                   movel     w0140         walOrc
071400100621     C                   movel     g02inv        waldac
071500100621     C                   eval      WalSIF = KNSIF
071600100621     C                   eval      WalPGM = 'TRTC87R   '
071700100621     C                   eval      WalNOJ = KNMEB
071800100621     C                   eval      WalNUS = KNMUS
071900100621     C                   eval      WalNRJ = KNRJO
072000100621     C                   eval      WalFLD = 'FILIALE   ' + %editc(UTEFIL:'X')
072100100622     c                                      + ' - ATR'
072200100621     C                   write     Fiwalc00
072300100621     c                   feod      Fiwalc1L
072400100621     C***
072500961003     C*
072600961003     C                   ENDSR
072700980320**
072800961008Punto operativo inesistente o annullato                                01
072900961008Punto operativo non gestito su questo AS                               02
073000961008Il punto operativo non si gestisce in modo autonomo                    03
073100961008Codice cliente inesistente o annullato                                 04
073200961008Codice cliente non relativo ad un partner                              05
073300980401Per cliente/Lnp selezionato non ci sono date di arrivo da trasmettere  06
073400980401Per cliente/Lnp selezionato non devo trasmettere le date di arrivo     07
073500980320Selezionare un codice cliente o un linea di partenza di un partner     08
073600980320Impossibile selezionare entrambi (sia lnp che codice cliente)          09
073700980320La Lnp non esiste su tabella EDI o non appartiene ad un partner        10
