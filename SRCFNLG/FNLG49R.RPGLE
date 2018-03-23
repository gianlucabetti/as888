000100951003     H DECEDIT('0,') DATEDIT(*DMY.)
000200951003?     *--------------------------------------------------------------*
000300951003      * FNLG49R                                                      *
000400951003      *     *----------------------------------------------*         *
000500951003      *            STATISTICA GIACENZE PER CLIENTE                   *
000600951003      *                     UFFICIO GIACENZE                         *
000700951003      *     *---------------------------------------------*          *
000800951003      *                                                              *
000900951003?     *--------------------------------------------------------------*
001000960209      * 01 - Lavoro x codice cliente                                 *
001100960209      * 02 - Lavoro x lnp                                            *
001200960209      * 03 - Lavoro x AREA                                           *
001300951003      * 30 - Lettura FNBLP26C                                        *
001400951003      * 31 - Chain varie + lokup                                     *
001500951003      * 32 - Chain archivio giacenze                                 *
001600951003?     *--------------------------------------------------------------*
001700020719     FFNBLP76L  IF   E           K DISK
001800951003     F*--------
001900050309     Ftigcp01L  IF   E           K DISK
002000951003     F*--------
002100021011     FAZORG01L  IF   E           K DISK
002200951003     F*--------
002300951003     FTABEL00F  IF   E           K DISK    USROPN
002400951003     F*--------
002500951003     FCNACO00F  IF   E           K DISK    USROPN
002600951003     F*--------
002700951003     FFNLG49P   O    E             PRINTER INFDS(STAMP)
002800951003      *---------
002900951003     FSYSPRT    O    F  132        PRINTER
003000951003     D*---------------------------------------------------------------*
003100951003     D* SCHIERA
003200951003     D*---------------------------------------------------------------*
003300951003     D EVE             S              3    DIM(100)
003400951003     D STT             S             66    DIM(2) CTDATA PERRCD(1)
003500951003?     *--------------------------------------------------------------*
003600951003?     *  DS                                                          *
003700951003?     *--------------------------------------------------------------*
003800951003     D*  DS x gestione contatore di righe
003900951003     D STAMP           DS
004000951003     D  LIN                  367    368B 0
004100951003     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
004200021011     D  TCU                  398    697    DIM(50)
004300021011     D  KCU                  698    847P 0 DIM(50)
004400951003     D                                     PACKEVEN
004500951003     D TCUDS           DS
004600951003     D  F34                    3      4
004700951003     D  F56                    5      6
004800951003     D CNCR80        E DS
004900021011      *
005000020719     d trul06ds      e ds
005100020719     d  linp                   1     90  0 dim(30)
005200021011      *
005300951003     D KPJBA         E DS
005400951003     D FNLG48        E DS                  EXTNAME(FNLG48DS)
005500021011     D  Ksc                   17    156  0 dim(20)
005600021011      *
005700021010     D kpjbus          S                   LIKE(kpjbu)
005800021011      *
005900951003      *  Ds multipla x memorizzazione totali statistica
006000041006     D MULT            DS           123    OCCURS(100)
006100951003     D  WDESEV                 1     75
006200951003     D  WGCSPE                76     87  0
006300951003     D  WTOTAP                88     99  0
006400041006     D  WGCcsp               100    111  0
006500041006     D  WGCnsp               112    123  0
006600951010      *  Ds multipla x TOTALI
006700041006     D TOT             DS           123    OCCURS(100)
006800951010     D  WTDESE                 1     75
006900951010     D  WTGIAC                76     87  0
007000951010     D  WTOGAP                88     99  0
007100041006     D  Wtgcsp               100    111  0
007200041006     D  Wtgnsp               112    123  0
007300951003     D WLBDA8          DS
007400951003     D  G02DAT                 1      8  0
007500951003     D  G02INV                 9     16  0
007600951003     D  G02ERR                17     17
007700951003     D  G02TGI                18     22  0
007800951003     D                SDS
007900951003     D  NOMPGM                 1     10
008000021011      *
008100021011      *  Definisco variabili
008200021011     d wlna                                like(blplna)
008300021011     d klna                                like(blplna)
008400021011     d kaas                                like(blpaas)
008500021011     d da_aas                              like(blpaas)
008600021011     d da_mgs                              like(blpmgs)
008700021011     d kmgs                                like(blpmgs)
008800021011     d klnp                                like(gcplnp)
008900021011     d knrs                                like(gcpnrs)
009000021011     d knsp                                like(gcpnsp)
009100021011     d kfrg                                like(gcpfrg)
009200021011     d kkut                                like(tblkut)
009300021011     d kcod                                like(tblcod)
009400021011     d kkcc                                like(acokcc)
009500021011     d kksc                                like(acoksc)
009600041006      *
009700021011     d wtotsp                              like(totspe)
009800021011     d wtotgc                              like(totgia)
009900021011     d wtotrs                              like(totres)
010000021011     d wtotga                              like(totgap)
010100041006     d wtotcs                              like(totcsp)
010200041006     d wtotns                              like(totnsp)
010300041006      *
010400021011     d wttspe                              like(totspe)
010500021011     d wttgia                              like(totgia)
010600021011     d wttres                              like(totres)
010700021011     d wttgap                              like(totgap)
010800041006     d wttcsp                              like(totcsp)
010900041006     d wttnsp                              like(totnsp)
011000041006      *
011100041006     d tspese          S             11  3
011200951003?     *--------------------------------------------------------------*
011300951003?     *  CICLO PRINCIPALE                                            *
011400951003?     *--------------------------------------------------------------*
011500021011      *  Esegeue un ciclo per LNA x filiali
011600021011     c     *start        setll     azorg01l
011700021011     c                   read      azorg01l
011800021011      *
011900021011     c                   dow       not %eof(azorg01l)
012000021011      *  solo quelli validi
012100021011     c                   if        orgfva = *blank and
012200021011     c                             (orgfag = 'F' or orgfag = 'A')
012300021011      *  Inzializzo dati x lna
012400021011     C                   EXSR      INZLNA
012500021011      *
012600021011      *  Esegeue un ciclo per LNP da 1 a 30
012700021011     c                   do        30            lnp30             3 0
012800021011      *  quando ha esaurito le LNP esce da questo ciclo x leggere una
012900021011      *  nuova LNA
013000021011     c                   if        linp(lnp30) = 0
013100021011     c                   leave
013200021011     c                   endIF
013300021011      *
013400021011      * elaborazione x LNP
013500021011      *  seleziona solo le linee dell'area richieste
013600021011      * posizionandosi ottimizzando il giro.
013700021011      *  altrimenti
013800021011      * elaborazione x Cliente
013900021011      *  deve leggerle tutte scartando x cliente.
014000021011     c                   if        d48mod <> 'C'
014100021011     c                   z-add     linp(lnp30)   wlinp             7 0
014200021011     c     wlinp         lookup    Ksc                                    31
014300021011     c  n31              goto      lnp_new
014400021011     c                   end
014500021011      *
014600021011      * posizionamento iniziale di lettura
014700021011      *  x chiave completa con data inizio
014800021011      *    e lettura x LNP/LNA
014900021011     c                   z-add     linp(lnp30)   xlnp              3 0
015000021011     C     Key_set       Setll     FnBlp76l
015100021011     C     Key_Lna       reade     FnBlp76l                               30
015200021011      *
015300021011      * Controlla limite data superiore
015400021011     C                   Exsr      Chkdata
015500021011      *
015600021011      *  Legge fino alla data
015700021011     c     *in30         doweq     *Off
015800021011     C     WDTSPE        ANDLE     D48DSA
015900021011      *
016000021011      *     Dettaglio di calcolo x totali da stampare
016100021011     C                   EXSR      GESLNA
016200021011      *
016300021011     C     Key_Lna       reade     FnBlp76l                               30
016400021011      * Controlla limite data superiore
016500021011     C                   Exsr      Chkdata
016600021011     c                   enddo
016700021011      *
016800021011     c     lnp_new       tag
016900021011      * lnpx
017000021011     C                   ENDdo
017100021011      *
017200021011      *  Totali per linea di arrivo
017300021011     C                   EXSR      TOTLIN
017400021011     c                   endIF
017500021011      * lnax
017600021011     c                   read      azorg01l
017700021011     C                   ENDdo
017800021011      *
017900951010      *  Se ho trovato almeno una sped. per il cliente stampo tot.gener.
018000951010     C     WTTSPE        IFNE      0
018100951010     C                   EXSR      TOTGEN
018200951010     C                   END
018300951003      *  Fine Lavoro
018400951003     C     FINE          TAG
018500951003     C                   SETON                                        LR
018600020723?     *--------------------------------------------------------------*
018700021011?     *  Controlla se non è andato oltre la data limite superiore    *
018800020723?     *--------------------------------------------------------------*
018900021011     C     ChkData       BEGSR
019000021011      *
019100021011     c                   if        *in30 = *off
019200021011     C                   Z-ADD     BLPMGS        WDTSPE            8 0
019300020723     C                   MOVEL     BLPAAS        WDTSPE
019400020723     C                   MOVEL     BLPLNA        WLNA
019500020723     C                   Z-ADD     BLPLNA        KLNA
019600020724     c                   end
019700020724      *
019800021011     C                   ENDSR
019900951003?     *--------------------------------------------------------------*
020000951003?     *  Operazione di inizializzazione totali x lna                 *
020100951003?     *--------------------------------------------------------------*
020200951003     C     INZLNA        BEGSR
020300951003      *
020400951003     C                   Z-ADD     1             X                 4 0
020500951003     C     X             DOWLE     100
020600951003     C     EVE(X)        ANDNE     *BLANKS
020700951003     C     X             OCCUR     MULT
020800951003     C                   Z-ADD     0             WGCSPE
020900951003     C                   Z-ADD     0             WTOTAP
021000041006     C                   Z-ADD     0             wgcCSP
021100041006     C                   Z-ADD     0             wgcNSP
021200951003     C                   ADD       1             X
021300951003     C                   END
021400021011      *
021500951003     C                   Z-ADD     0             WTOTSP
021600951003     C                   Z-ADD     0             WTOTGC
021700951003     C                   Z-ADD     0             WTOTRS
021800951003     C                   Z-ADD     0             WTOTGA
021900041006     C                   Z-ADD     0             WTOTcs
022000041006     C                   Z-ADD     0             WTOTns
022100951003      *  Decodifico lna
022200021011     C                   Z-ADD     ORGFIL        xlna              3 0
022300951003     C                   MOVEL     ORGDES        DESLNA
022400951003     C                   Z-ADD     ORGFIL        CODLNA
022500951003      *
022600951003     C                   ENDSR
022700951003?     *--------------------------------------------------------------*
022800951003?     *  Gestione singola lna                                        *
022900951003?     *--------------------------------------------------------------*
023000951003     C     GESLNA        BEGSR
023100951003      *
023200960209      *  Controllo se devo testare cliente o lnp
023300960209     C     D48MOD        IFEQ      'C'
023400951003      *  Controllo se cliente letto è da considerare
023500951003     C     BLPCCM        IFNE      0
023600951003     C     BLPCCM        LOOKUP    KSC                                    31
023700951003     C                   ELSE
023800951003     C     BLPKSC        LOOKUP    KSC                                    31
023900951003     C                   END
024000960209     C*
024100960209     C                   ELSE
024200960209      *  Controllo se lnp letta è da considerare
024300960209     C                   Z-ADD     BLPLNP        WLNP              7 0
024400960209     C     WLNP          LOOKUP    KSC                                    31
024500960209     C                   END
024600960209     C*
024700951003     C     *IN31         IFEQ      '1'
024800951003     C                   ADD       1             WTOTSP
024900951003      *  Controllo se cliente ha avuto giacenza
025000951003     C                   Z-ADD     BLPAAS        KAAS
025100951003     C                   Z-ADD     BLPLNP        KLNP
025200951003     C                   Z-ADD     BLPNRS        KNRS
025300951003     C                   Z-ADD     BLPNSP        KNSP
025400951003     C                   Z-ADD     0             KFRG
025500050309     C     KGCP          CHAIN     tigcp01L                           32
025600951003     C     *IN32         IFEQ      '0'
025700951003     C                   ADD       1             WTOTGC
025800951003     C     GCPDCG        IFEQ      0
025900951003     C                   ADD       1             WTOTGA
026000951009     C                   END
026100951003     C     GCPDIS        IFEQ      'R'
026200951003     C                   ADD       1             WTOTRS
026300951003     C                   END
026400041006      **  giacenza chiusa
026500041006     C                   If        gcpDCG > 0 and gcpFAS >= 50
026600041006      ** controllo se ha spese
026700041006     c                   eval      tspese = gcpSGS + gcpSGR + gcpSGD + gcpSGA
026800041006      *  se ci sono degli importi ed è stata creata la bolla allora
026900041006      *   ci sono delle spese
027000041006     c                   if        tspese > 0 and gcpNBW > 0
027100041006     C                   ADD       1             WTOTcs
027200041006     C                   else
027300041006     C                   ADD       1             WTOTns
027400041006     C                   end
027500041006     C                   end
027600041006      **
027700951003     C                   END
027800951003     C                   END
027900960209     C*
028000951003      *  Se cliente da considerare e trovato giacenza incremento
028100951003      *  totali motivi giacenza
028200951003     C     *IN31         IFEQ      '1'
028300951003     C     *IN32         ANDEQ     '0'
028400951003     C                   Z-ADD     1             X
028500951003     C     GCPCMC        LOOKUP    EVE(X)                                 31
028600951005     C     X             OCCUR     MULT
028700951003     C     *IN31         IFEQ      '1'
028800951003     C                   ADD       1             WGCSPE
028900951003     C     GCPDCG        IFEQ      0
029000951003     C                   ADD       1             WTOTAP
029100951003     C                   END
029200041006      **  giacenza chiusa
029300041006     C                   If        gcpDCG > 0 and gcpFAS >= 50
029400041006      ** controllo se ha spese
029500041006     c                   eval      tspese = gcpSGS + gcpSGR + gcpSGD + gcpSGA
029600041006      *  se ci sono degli importi ed è stata creata la bolla allora
029700041006      *   ci sono delle spese
029800041006     c                   if        tspese > 0 and gcpNBW > 0
029900041006     C                   ADD       1             wgcCSP
030000041006     C                   else
030100041006     C                   ADD       1             wgcNSP
030200041006     C                   end
030300041006     C                   end
030400041006      **
030500951003     C                   END
030600951003     C                   END
030700951003     C*
030800951003     C                   ENDSR
030900951003?     *--------------------------------------------------------------*
031000951003?     *  Operazione di inizializzazione totali x lna                 *
031100951003?     *--------------------------------------------------------------*
031200951003     C     TOTLIN        BEGSR
031300951003      *
031400951003     C                   MOVEL     'N'           WSTMOT            1
031500951003      *  Scrivo totali x cliente/lna
031600951003     C     WTOTSP        IFNE      0
031700951003     C     WPRPAG        IFNE      'N'
031800951006     C     LIN           ORGT      63
031900951003     C                   MOVEL     'N'           WPRPAG            1
032000951003     C                   WRITE     TESGEN
032100951003     C                   WRITE     TESCLI
032200951003     C                   END
032300951003     C                   WRITE     TESLNA
032400951003     C*
032500951003     C                   Z-ADD     1             X
032600951003     C     X             DOWLE     100
032700951003     C     EVE(X)        ANDNE     *BLANKS
032800951003     C     X             OCCUR     MULT
032900951003     C     WGCSPE        IFNE      *ZEROS
033000951003     C                   Z-ADD     WGCSPE        GIAEVE
033100951003     C                   Z-ADD     WTOTAP        APEEVE
033200041006     C                   Z-ADD     wgcCSP        conspe
033300041006     C                   Z-ADD     wgcNSP        senspe
033400951010     C     X             OCCUR     TOT
033500951010     C                   ADD       WGCSPE        WTGIAC
033600951010     C                   ADD       WTOTAP        WTOGAP
033700041006     C                   ADD       wgcCSP        wtgCSP
033800041006     C                   ADD       wgcNSP        wtgNSP
033900951003     C                   MOVEL     EVE(X)        CODEVE
034000951003     C                   MOVEL     WDESEV        DESEVE
034100951003     C     WSTMOT        IFEQ      'N'
034200951006     C     LIN           IFGT      61
034300951005     C                   WRITE     TESGEN
034400951005     C                   WRITE     TESCLI
034500951005     C                   WRITE     TESLNA
034600951005     C                   END
034700951005     C                   MOVEL     'S'           WSTMOT            1
034800951005     C                   WRITE     TESMOT
034900951005     C                   ELSE
035000951006     C     LIN           IFGT      64
035100951005     C                   WRITE     TESGEN
035200951005     C                   WRITE     TESCLI
035300951005     C                   WRITE     TESLNA
035400951005     C                   WRITE     TESMOT
035500951005     C                   END
035600951003     C                   END
035700951003     C                   WRITE     DETTAG
035800951003     C                   END
035900951003     C                   ADD       1             X
036000951003     C                   END
036100951003      *  Stampo totale lna
036200951005     C     LIN           IFGT      62
036300951003     C                   WRITE     TESGEN
036400951003     C                   WRITE     TESCLI
036500951003     C                   WRITE     TESLNA
036600951003     C                   END
036700951003     C                   Z-ADD     WTOTSP        TOTSPE
036800951003     C                   Z-ADD     WTOTGC        TOTGIA
036900951003     C                   Z-ADD     WTOTRS        TOTRES
037000951003     C                   Z-ADD     WTOTGA        TOTGAP
037100041006     C                   Z-ADD     WTOTcs        totCSP
037200041006     C                   Z-ADD     WTOTns        totNSP
037300951010     C                   ADD       WTOTSP        WTTSPE
037400951010     C                   ADD       WTOTGC        WTTGIA
037500951010     C                   ADD       WTOTRS        WTTRES
037600951010     C                   ADD       WTOTGA        WTTGAP
037700041006     C                   add       WTOTcs        wttCSP
037800041006     C                   add       WTOTns        wttNSP
037900041006      *  Calcolo percentuali
038000041006     c                   exsr      CALC_PERC
038100951003     C                   END
038200951003      *
038300951003     C                   ENDSR
038400951010?     *--------------------------------------------------------------*
038500951010?     *  Operazione di stampa totali generali                        *
038600951010?     *--------------------------------------------------------------*
038700951010     C     TOTGEN        BEGSR
038800951010      *
038900951010     C                   MOVEL     'N'           WSTMOT            1
039000951010      *  Scrivo totali x cliente/lna
039100951010     C     WPRPAG        IFNE      'N'
039200951010     C     LIN           ORGT      63
039300951010     C                   MOVEL     'N'           WPRPAG            1
039400951010     C                   WRITE     TESGEN
039500951010     C                   WRITE     TESCLI
039600951010     C                   END
039700951010     C                   WRITE     TESTOT
039800951010     C*
039900951010     C                   Z-ADD     1             X
040000951010     C     X             DOWLE     100
040100951010     C     EVE(X)        ANDNE     *BLANKS
040200951010     C     X             OCCUR     TOT
040300951010     C     WTGIAC        IFNE      *ZEROS
040400951010     C                   Z-ADD     WTGIAC        GIAEVE
040500951010     C                   Z-ADD     WTOGAP        APEEVE
040600041006     C                   Z-ADD     wtgCSP        conSPE
040700041006     C                   Z-ADD     wtgNSP        senSPE
040800951010     C                   MOVEL     EVE(X)        CODEVE
040900951010     C                   MOVEL     WTDESE        DESEVE
041000951010     C     WSTMOT        IFEQ      'N'
041100951010     C     LIN           IFGT      61
041200951010     C                   WRITE     TESGEN
041300951010     C                   WRITE     TESCLI
041400951010     C                   WRITE     TESTOT
041500951010     C                   END
041600951010     C                   MOVEL     'S'           WSTMOT            1
041700951010     C                   WRITE     TESMOT
041800951010     C                   ELSE
041900951010     C     LIN           IFGT      64
042000951010     C                   WRITE     TESGEN
042100951010     C                   WRITE     TESCLI
042200951010     C                   WRITE     TESTOT
042300951010     C                   WRITE     TESMOT
042400951010     C                   END
042500951010     C                   END
042600951010     C                   WRITE     DETTAG
042700951010     C                   END
042800951010     C                   ADD       1             X
042900951010     C                   END
043000951010      *  Stampo totale lna
043100951010     C     LIN           IFGT      62
043200951010     C                   WRITE     TESGEN
043300951010     C                   WRITE     TESCLI
043400951010     C                   WRITE     TESTOT
043500951010     C                   END
043600951010     C                   Z-ADD     WTTSPE        TOTSPE
043700951010     C                   Z-ADD     WTTGIA        TOTGIA
043800951010     C                   Z-ADD     WTTRES        TOTRES
043900951010     C                   Z-ADD     WTTGAP        TOTGAP
044000041006     C                   Z-ADD     wttCSP        TOTCSP
044100041006     C                   Z-ADD     wttNSP        TOTNSP
044200041006?     *  Calcolo Percentuali
044300041006     c                   exsr      CALC_PERC
044400041006      *
044500041006     C                   ENDSR
044600041006?     *--------------------------------------------------------------*
044700041006?     *  Calcolo Percentuali                                         *
044800041006?     *--------------------------------------------------------------*
044900041006     C     CALC_PERC     BEGSR
045000041006      *
045100041006      *  Calcolo percentuali
045200041006     C     TOTGIA        MULT      100           WCOM14           14 0
045300041006     C     WCOM14        DIV(H)    TOTSPE        SPEGIA
045400041006      *
045500041006     C     TOTRES        MULT      100           WCOM14           14 0
045600041006     C     TOTGIA        IFNE      0
045700041006     C     WCOM14        DIV(H)    TOTGIA        GIARES
045800041006     C                   ELSE
045900041006     C                   Z-ADD     0             GIARES
046000041006     C                   END
046100041006      *
046200041006     C     totGAP        MULT      100           Wcom14           14 0
046300041006     C     totGIA        IFNE      0
046400041006     C     Wcom14        DIV(H)    totGIA        NCHGIA
046500041006     C                   ELSE
046600041006     C                   Z-ADD     0             NCHGIA
046700041006     C                   END
046800041006      *
046900041006     C     totCSP        MULT      100           Wcom14           14 0
047000041006     C     totGIA        IFNE      0
047100041006     C     Wcom14        DIV(H)    totGIA        CSPGIA
047200041006     C                   ELSE
047300041006     C                   Z-ADD     0             CSPGIA
047400041006     C                   END
047500041006      *
047600041006      *
047700041006     C     totNSP        MULT      100           Wcom14           14 0
047800041006     C     totGIA        IFNE      0
047900041006     C     Wcom14        DIV(H)    totGIA        NSPGIA
048000041006     C                   ELSE
048100041006     C                   Z-ADD     0             NSPGIA
048200041006     C                   END
048300041006      *
048400041006     C                   WRITE     TOTLNA
048500951010      *
048600951010     C                   ENDSR
048700951003?     *--------------------------------------------------------------*
048800951003?     *  Operazioni iniziali                                         *
048900951003?     *--------------------------------------------------------------*
049000951003     C     *INZSR        BEGSR
049100951003      *
049200951003     C     *ENTRY        PLIST
049300951003     C                   PARM                    KPJBA
049400951003     C                   MOVEL     KPJBU         FNLG48
049500951003     C* Richiamo XPARUT
049600951003     C                   Z-ADD     1             CODUT
049700951003     C                   CALL      'X§PARUT'
049800951003     C                   PARM                    UTEDSE
049900951003     C                   MOVEL     RAGUT         RSUT
050000951003     C                   MOVEL     REC80         CNCR80
050100951003     C                   MOVEL     SIMFEL        STPFEL
050200020719      *
050300020719      * reperisce le linee di partenza dell'area
050400020719     c                   clear                   trul06ds
050500021010     C                   MOVEL     kpjbu         KPJBUs
050600020719     c                   eval      d06cod = '£1'
050700020719     c                   movel     simfel        d06key
050800020719     c                   movel     trul06ds      kpjbu
050900020719     c                   call      'TRUL06R'
051000020719     c                   parm                    kpjba
051100020719     c                   movel     kpjbu         trul06ds
051200021010     C                   MOVEL     kpjbus        KPJBU
051300020719      *
051400951003     C*--- Ricerca Capoconti -----------------------------------------*
051500951003     C                   DO        50            X
051600951003     C                   MOVE      TCU(X)        TCUDS
051700951003     C     F56           IFEQ      'CG'
051800951003     C     F34           ANDEQ     '01'
051900951003     C                   Z-ADD     KCU(X)        CLIITA            4 0
052000951003     C                   END
052100951003     C                   END
052200021011      *
052300951003      *  Definisco chiavi di accesso ai file
052400021011     C     Key_set       KLIST
052500021011     C                   KFLD                    xlnp
052600021011     C                   KFLD                    xlna
052700021011     C                   KFLD                    da_aas
052800021011     C                   KFLD                    da_mgs
052900021011      *
053000021011     C     Key_Lna       KLIST
053100021011     C                   KFLD                    xlnp
053200021011     C                   KFLD                    xlna
053300021011      *
053400951003     C     KGCP          KLIST
053500951003     C                   KFLD                    KAAS
053600951003     C                   KFLD                    KLNP
053700951003     C                   KFLD                    KNRS
053800951003     C                   KFLD                    KNSP
053900951003     C                   KFLD                    KFRG
054000021011      *
054100951003     C     KTAB          KLIST
054200951003     C                   KFLD                    KKUT
054300951003     C                   KFLD                    KCOD
054400021011      *
054500951003     C     KACO          KLIST
054600951003     C                   KFLD                    KKUT
054700951003     C                   KFLD                    KKCC
054800951003     C                   KFLD                    KKSC
054900021011      *
055000951003      *  Carico tabella codici eventi
055100951003     C                   Z-ADD     0             X
055200951003     C                   Z-ADD     1             KKUT
055300951003     C                   MOVEL     '2A'          KCOD
055400951003     C                   OPEN      TABEL00F
055500951003     C     KTAB          CHAIN     TABEL00F                           31
055600951003     C     *IN31         DOWEQ     '0'
055700951003     C     X             ANDLT     100
055800951003     C                   ADD       1             X
055900951003     C                   MOVEL     TBLKEY        EVE(X)
056000951003     C     X             OCCUR     MULT
056100951003     C                   MOVEL     TBLUNI        WDESEV
056200951010     C     X             OCCUR     TOT
056300951010     C                   MOVEL     TBLUNI        WTDESE
056400951003     C     KTAB          READE     TABEL00F                               31
056500951003     C                   END
056600951003     C                   CLOSE     TABEL00F
056700951003     C     *IN31         IFEQ      '0'
056800951003     C                   EXCEPT    TESERR
056900951003     C                   EXCEPT    DETERR
057000951003     C                   GOTO      FINE
057100951003     C                   END
057200021011      *
057300960209     C                   SETOFF                                       010203
057400021011      *
057500960209      *  Controllo se devo decodificare il cliente
057600960209     C     D48MOD        IFEQ      'C'
057700960209     C                   SETON                                        01
057800960209      *  Decodifico cod.cliente
057900951003     C                   OPEN      CNACO00F
058000951003     C                   Z-ADD     CLIITA        KKCC
058100951003     C                   Z-ADD     KSC(1)        KKSC
058200951003     C                   MOVEL     *BLANKS       ACORAG
058300951003     C     KACO          CHAIN     CNACO00F                           31
058400951003     C                   MOVEL     ACORAG        DESCLI
058500960212     C                   ELSE
058600960209      *  Controllo se devo decodificare lnp o area
058700960209     C     KSC(2)        IFNE      0
058800960209     C                   SETON                                        03
058900960209     C                   Z-ADD     SIMFEL        ORGFIL
059000960209     C                   ELSE
059100960212     C                   SETON                                        02
059200960209     C                   Z-ADD     KSC(1)        ORGFIL
059300960209     C                   END
059400960209     C                   MOVEL     *BLANKS       ORGDES
059500960209     C     ORGFIL        CHAIN     AZORG01L                           31
059600960209     C                   MOVEL     ORGDES        DESCLI
059700960209     C                   END
059800960209      *
059900951003      *  Giro le date spedizioni
060000951003     C                   Z-ADD     D48DSD        G02INV
060100951003     C                   MOVE      '3'           G02ERR
060200951003     C                   CALL      'XSRDA8'
060300951003     C                   PARM                    WLBDA8
060400951003     C                   Z-ADD     G02DAT        DATADA
060500951003     C                   Z-ADD     D48DSA        G02INV
060600951003     C                   MOVE      '3'           G02ERR
060700951003     C                   CALL      'XSRDA8'
060800951003     C                   PARM                    WLBDA8
060900951003     C                   Z-ADD     G02DAT        DATAAL
061000951003     C                   TIME                    TIMES             6 0
061100021011      *
061200951003     C                   Z-ADD     D48DSD        KMGS
061300951003     C                   MOVEL     D48DSD        KAAS
061400021011      *
061500021011     C                   Z-ADD     D48DSD        da_mgs
061600021011     C                   MOVEL     D48DSD        da_aas
061700021011      *
061800041006     C                   MOVEL     *ALL'-'       CMP198
061900021011      *
062000951010      *  Azzero DS multipla totali
062100951010     C                   Z-ADD     1             X                 4 0
062200951010     C     X             DOWLE     100
062300951010     C     EVE(X)        ANDNE     *BLANKS
062400951010     C     X             OCCUR     TOT
062500951010     C                   Z-ADD     0             WTGIAC
062600951010     C                   Z-ADD     0             WTOGAP
062700041006     C                   Z-ADD     0             Wtgcsp
062800041006     C                   Z-ADD     0             Wtgnsp
062900951010     C                   ADD       1             X
063000951010     C                   END
063100951010     C                   Z-ADD     0             WTTSPE
063200951010     C                   Z-ADD     0             WTTGIA
063300951010     C                   Z-ADD     0             WTTRES
063400951010     C                   Z-ADD     0             WTTGAP
063500041006     C                   Z-ADD     0             WTTcsp
063600041006     C                   Z-ADD     0             WTTnsp
063700021011      *  Eseguo stampa prima pagina
063800021011     C                   EXSR      STPINI
063900951010      *
064000951003     C                   ENDSR
064100021011?     *--------------------------------------------------------------*
064200021011?     *  STPINI: Stampa prima pagina                                 *
064300021011?     *--------------------------------------------------------------*
064400021011     C     STPINI        BEGSR
064500021011      *
064600021011     c                   clear                   y                 3 0
064700021011      *
064800021011     C                   WRITE     PRIPAG
064900021011     C                   WRITE     DETSPA
065000021011      *
065100021011     C                   DO        20            X
065200021011      *
065300021011     C     KSC(X)        IFNE      0
065400021011     C                   Z-ADD     KSC(X)        STPKSC
065500021011     c                   add       1             y
065600021011      *  x cliente
065700021011     C     D48MOD        IFEQ      'C'
065800021011     C                   Z-ADD     CLIITA        KKCC
065900021011     C                   Z-ADD     KSC(X)        KKSC
066000021011     C                   MOVEL     *BLANKS       ACORAG
066100021011     C     KACO          CHAIN     CNACO00F                           31
066200021011     C                   MOVEL     ACORAG        STPDES
066300021011     C                   ELSE
066400021011      *  x LNP
066500021011     C                   Z-ADD     KSC(X)        ORGFIL
066600021011     C                   MOVEL     *BLANKS       ORGDES
066700021011     C     ORGFIL        CHAIN     AZORG01L                           31
066800021011     C                   MOVEL     ORGDES        STPDES
066900021011     C                   END
067000021011      *
067100021011     C                   WRITE     DETCLI
067200021011     C                   WRITE     DETSPA
067300021011      *
067400021011     C                   END
067500021011     C                   END
067600021011     C*
067700021011     C                   WRITE     FINPAG
067800021011     C                   CLOSE     CNACO00F
067900021011     C*
068000021011     C                   ENDSR
068100951003      *--------------------------------------------------------------*
068200951003     O*  STAMPA ERRORI !!!!!!!!
068300951003      *--------------------------------------------------------------*
068400951003     OSYSPRT    E            TESERR           01
068500951003     O                       STT(1)              66
068600951003     O                       STT(2)             132
068700951003     O                       RSUT                20
068800951003     O                       UDATE              118 '  /  /  '
068900951003     O                       PAGE          Z    132
069000951003     O          E            TESERR           03
069100951003     O                       TIMES              118 '  .  .  '
069200951003     O          E            TESERR           06
069300951003     O                                           24 'SI RACCOMANDA DI CONSEGN'
069400951003     O                                           48 'ARE QUESTA STAMPA ALL''U'
069500951003     O                                           72 'FFICIO  C.E.D.!!!!!!    '
069600951003     O          E            TESERR      0  2
069700951003     O                                           24 'SI RACCOMANDA DI CONSEGN'
069800951003     O                                           48 'ARE QUESTA STAMPA ALL''U'
069900951003     O                                           72 'FFICIO  C.E.D.!!!!!!    '
070000951003     O          E            DETERR         1
070100951003     O                                           24 'NON CARICATI TUTTI GLI E'
070200951003     O                                           48 'LEMENTI DELLA TABELLA 2A'
070300951003      *--------------------------------------------------------------*
070400990628** STT
070500951003                                 ***  S T A M P A     G I A C E N
070600990628Z E      P E R     C L I E N T E  ***  FLFG49R              PAG.
