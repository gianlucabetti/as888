000100951003     H DECEDIT('0,') DATEDIT(*DMY.)
000200951003?     *--------------------------------------------------------------*
000300070703      * FNLG53R                                                      *
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
001700070703     Fwgcpkl0f  IF   E           K DISK
001800951003     F*--------
001900070703     Ftigcp51L  IF   E           K DISK
002000951003     F*--------
002100021011     FAZORG01L  IF   E           K DISK
002200951003     F*--------
002300951003     FTABEL00F  IF   E           K DISK    USROPN
002400951003     F*--------
002500951003     FCNACO00F  IF   E           K DISK    USROPN
002600951003     F*--------
002700070703     FFNLG53P   O    E             PRINTER INFDS(STAMP)
002800951003      *---------
002900951003     FSYSPRT    O    F  132        PRINTER
003000951003     D*---------------------------------------------------------------*
003100951003     D* SCHIERA
003200951003     D*---------------------------------------------------------------*
003300090409     D EVE             S              3    DIM(200)
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
006000090409     D MULT            DS           123    OCCURS(200)
006100951003     D  WDESEV                 1     75
006200951003     D  WGCSPE                76     87  0
006300951003     D  WTOTAP                88     99  0
006400041006     D  WGCcsp               100    111  0
006500041006     D  WGCnsp               112    123  0
006600951010      *  Ds multipla x TOTALI
006700090409     D TOT             DS           123    OCCURS(200)
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
008200070703     d wlna                                like(gcplna)
008300070703     d klna                                like(gcplna)
008400070703     d kaas                                like(gcpaas)
008500070703     d da_aas                              like(gcpaas)
008600070703     d da_mgs                              like(gklmgs)
008700070703     d kmgs                                like(gklmgs)
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
015000070703     C     Key_set       Setll     wgcpkl0f
015100070703     C     Key_Lna       reade     wgcpkl0f                               30
015200021011      *  Legge fino alla data
015300021011     c     *in30         doweq     *Off
015400021011      *
015500021011      *     Dettaglio di calcolo x totali da stampare
015600021011     C                   EXSR      GESLNA
015700021011      *
015800070703     C     Key_Lna       reade     wgcpkl0f                               30
015900021011      * Controlla limite data superiore
016000021011     c                   enddo
016100021011      *
016200021011     c     lnp_new       tag
016300021011      * lnpx
016400021011     C                   ENDdo
016500021011      *
016600021011      *  Totali per linea di arrivo
016700021011     C                   EXSR      TOTLIN
016800021011     c                   endIF
016900021011      * lnax
017000021011     c                   read      azorg01l
017100021011     C                   ENDdo
017200021011      *
017300951010      *  Se ho trovato almeno una sped. per il cliente stampo tot.gener.
017400951010     C     WTTSPE        IFNE      0
017500951010     C                   EXSR      TOTGEN
017600951010     C                   END
017700951003      *  Fine Lavoro
017800951003     C     FINE          TAG
017900951003     C                   SETON                                        LR
018000951003?     *--------------------------------------------------------------*
018100951003?     *  Operazione di inizializzazione totali x lna                 *
018200951003?     *--------------------------------------------------------------*
018300951003     C     INZLNA        BEGSR
018400951003      *
018500951003     C                   Z-ADD     1             X                 4 0
018600090409     C     X             DOWLE     200
018700951003     C     EVE(X)        ANDNE     *BLANKS
018800951003     C     X             OCCUR     MULT
018900951003     C                   Z-ADD     0             WGCSPE
019000951003     C                   Z-ADD     0             WTOTAP
019100041006     C                   Z-ADD     0             wgcCSP
019200041006     C                   Z-ADD     0             wgcNSP
019300951003     C                   ADD       1             X
019400951003     C                   END
019500021011      *
019600951003     C                   Z-ADD     0             WTOTSP
019700951003     C                   Z-ADD     0             WTOTGC
019800951003     C                   Z-ADD     0             WTOTRS
019900951003     C                   Z-ADD     0             WTOTGA
020000041006     C                   Z-ADD     0             WTOTcs
020100041006     C                   Z-ADD     0             WTOTns
020200951003      *  Decodifico lna
020300021011     C                   Z-ADD     ORGFIL        xlna              3 0
020400951003     C                   MOVEL     ORGDES        DESLNA
020500951003     C                   Z-ADD     ORGFIL        CODLNA
020600951003      *
020700951003     C                   ENDSR
020800951003?     *--------------------------------------------------------------*
020900951003?     *  Gestione singola lna                                        *
021000951003?     *--------------------------------------------------------------*
021100951003     C     GESLNA        BEGSR
021200070831      *  Controllo se devo testare cliente o lnp
021300070831     C     D48MOD        IFEQ      'C'
021400070831      *  Controllo se cliente letto è da considerare
021500070831     C     gklccm        IFNE      0
021600070831     C     gklccm        LOOKUP    KSC                                    31
021700070831     C                   END
021800070831     C*
021900070831     C                   ELSE
022000070831      *  Controllo se lnp letta è da considerare
022100070831     C                   Z-ADD     gklLNP        WLNP              7 0
022200070831     C     WLNP          LOOKUP    KSC                                    31
022300070831     C                   END
022400070831     C*
022500070831     C     *IN31         IFEQ      '1'
022600951003      *
022700951003     C                   ADD       1             WTOTSP
022800951003      *  Controllo se cliente ha avuto giacenza
022900070703     C                   Z-ADD     gklAAS        KAAS
023000070703     C                   Z-ADD     gklLNP        KLNP
023100070703     C                   Z-ADD     gklNRS        KNRS
023200070703     C                   Z-ADD     gklNSP        KNSP
023300951003     C                   Z-ADD     0             KFRG
023400070703     C     KGCP          CHAIN     tigcp51L                           32
023500951003     C     *IN32         IFEQ      '0'
023600951003     C                   ADD       1             WTOTGC
023700951003     C     GCPDCG        IFEQ      0
023800951003     C                   ADD       1             WTOTGA
023900951009     C                   END
024000951003     C     GCPDIS        IFEQ      'R'
024100951003     C                   ADD       1             WTOTRS
024200951003     C                   END
024300041006      **  giacenza chiusa
024400041006     C                   If        gcpDCG > 0 and gcpFAS >= 50
024500041006      ** controllo se ha spese
024600041006     c                   eval      tspese = gcpSGS + gcpSGR + gcpSGD + gcpSGA
024700041006      *  se ci sono degli importi ed è stata creata la bolla allora
024800041006      *   ci sono delle spese
024900041006     c                   if        tspese > 0 and gcpNBW > 0
025000041006     C                   ADD       1             WTOTcs
025100041006     C                   else
025200041006     C                   ADD       1             WTOTns
025300041006     C                   end
025400041006     C                   end
025500041006      **
025600951003     C                   END
025700070831     C                   END
025800960209     C*
025900951003      *  Se cliente da considerare e trovato giacenza incremento
026000951003      *  totali motivi giacenza
026100070704     C     *IN31         IFEQ      '1'
026200070704     C     *IN32         andeq     '0'
026300951003     C                   Z-ADD     1             X
026400951003     C     GCPCMC        LOOKUP    EVE(X)                                 31
026500951005     C     X             OCCUR     MULT
026600951003     C     *IN31         IFEQ      '1'
026700951003     C                   ADD       1             WGCSPE
026800951003     C     GCPDCG        IFEQ      0
026900951003     C                   ADD       1             WTOTAP
027000951003     C                   END
027100041006      **  giacenza chiusa
027200041006     C                   If        gcpDCG > 0 and gcpFAS >= 50
027300041006      ** controllo se ha spese
027400041006     c                   eval      tspese = gcpSGS + gcpSGR + gcpSGD + gcpSGA
027500041006      *  se ci sono degli importi ed è stata creata la bolla allora
027600041006      *   ci sono delle spese
027700041006     c                   if        tspese > 0 and gcpNBW > 0
027800041006     C                   ADD       1             wgcCSP
027900041006     C                   else
028000041006     C                   ADD       1             wgcNSP
028100041006     C                   end
028200041006     C                   end
028300041006      **
028400951003     C                   END
028500951003     C                   END
028600951003     C*
028700951003     C                   ENDSR
028800951003?     *--------------------------------------------------------------*
028900951003?     *  Operazione di inizializzazione totali x lna                 *
029000951003?     *--------------------------------------------------------------*
029100951003     C     TOTLIN        BEGSR
029200951003      *
029300951003     C                   MOVEL     'N'           WSTMOT            1
029400951003      *  Scrivo totali x cliente/lna
029500951003     C     WTOTSP        IFNE      0
029600951003     C     WPRPAG        IFNE      'N'
029700070704     C     LIN           ORGT      58
029800951003     C                   MOVEL     'N'           WPRPAG            1
029900951003     C                   WRITE     TESGEN
030000951003     C                   WRITE     TESCLI
030100951003     C                   END
030200951003     C                   WRITE     TESLNA
030300951003     C*
030400951003     C                   Z-ADD     1             X
030500090409     C     X             DOWLE     200
030600951003     C     EVE(X)        ANDNE     *BLANKS
030700951003     C     X             OCCUR     MULT
030800951003     C     WGCSPE        IFNE      *ZEROS
030900951003     C                   Z-ADD     WGCSPE        GIAEVE
031000951003     C                   Z-ADD     WTOTAP        APEEVE
031100041006     C                   Z-ADD     wgcCSP        conspe
031200041006     C                   Z-ADD     wgcNSP        senspe
031300951010     C     X             OCCUR     TOT
031400951010     C                   ADD       WGCSPE        WTGIAC
031500951010     C                   ADD       WTOTAP        WTOGAP
031600041006     C                   ADD       wgcCSP        wtgCSP
031700041006     C                   ADD       wgcNSP        wtgNSP
031800951003     C                   MOVEL     EVE(X)        CODEVE
031900951003     C                   MOVEL     WDESEV        DESEVE
032000951003     C     WSTMOT        IFEQ      'N'
032100070704     C     LIN           IFGT      55
032200951005     C                   WRITE     TESGEN
032300951005     C                   WRITE     TESCLI
032400951005     C                   WRITE     TESLNA
032500951005     C                   END
032600951005     C                   MOVEL     'S'           WSTMOT            1
032700951005     C                   WRITE     TESMOT
032800951005     C                   ELSE
032900070704     C     LIN           IFGT      58
033000951005     C                   WRITE     TESGEN
033100951005     C                   WRITE     TESCLI
033200951005     C                   WRITE     TESLNA
033300951005     C                   WRITE     TESMOT
033400951005     C                   END
033500951003     C                   END
033600951003     C                   WRITE     DETTAG
033700951003     C                   END
033800951003     C                   ADD       1             X
033900951003     C                   END
034000951003      *  Stampo totale lna
034100070704     C     LIN           IFGT      57
034200951003     C                   WRITE     TESGEN
034300951003     C                   WRITE     TESCLI
034400951003     C                   WRITE     TESLNA
034500951003     C                   END
034600951003     C                   Z-ADD     WTOTSP        TOTSPE
034700951003     C                   Z-ADD     WTOTGC        TOTGIA
034800951003     C                   Z-ADD     WTOTRS        TOTRES
034900951003     C                   Z-ADD     WTOTGA        TOTGAP
035000041006     C                   Z-ADD     WTOTcs        totCSP
035100041006     C                   Z-ADD     WTOTns        totNSP
035200951010     C                   ADD       WTOTSP        WTTSPE
035300951010     C                   ADD       WTOTGC        WTTGIA
035400951010     C                   ADD       WTOTRS        WTTRES
035500951010     C                   ADD       WTOTGA        WTTGAP
035600041006     C                   add       WTOTcs        wttCSP
035700041006     C                   add       WTOTns        wttNSP
035800041006      *  Calcolo percentuali
035900041006     c                   exsr      CALC_PERC
036000951003     C                   END
036100951003      *
036200951003     C                   ENDSR
036300951010?     *--------------------------------------------------------------*
036400951010?     *  Operazione di stampa totali generali                        *
036500951010?     *--------------------------------------------------------------*
036600951010     C     TOTGEN        BEGSR
036700951010      *
036800951010     C                   MOVEL     'N'           WSTMOT            1
036900951010      *  Scrivo totali x cliente/lna
037000951010     C     WPRPAG        IFNE      'N'
037100070705     C     LIN           ORGT      55
037200951010     C                   MOVEL     'N'           WPRPAG            1
037300951010     C                   WRITE     TESGEN
037400951010     C                   WRITE     TESCLI
037500951010     C                   END
037600951010     C                   WRITE     TESTOT
037700951010     C*
037800951010     C                   Z-ADD     1             X
037900090409     C     X             DOWLE     200
038000951010     C     EVE(X)        ANDNE     *BLANKS
038100951010     C     X             OCCUR     TOT
038200951010     C     WTGIAC        IFNE      *ZEROS
038300951010     C                   Z-ADD     WTGIAC        GIAEVE
038400951010     C                   Z-ADD     WTOGAP        APEEVE
038500041006     C                   Z-ADD     wtgCSP        conSPE
038600041006     C                   Z-ADD     wtgNSP        senSPE
038700951010     C                   MOVEL     EVE(X)        CODEVE
038800951010     C                   MOVEL     WTDESE        DESEVE
038900951010     C     WSTMOT        IFEQ      'N'
039000070704     C     LIN           IFGT      55
039100951010     C                   WRITE     TESGEN
039200951010     C                   WRITE     TESCLI
039300951010     C                   WRITE     TESTOT
039400951010     C                   END
039500951010     C                   MOVEL     'S'           WSTMOT            1
039600951010     C                   WRITE     TESMOT
039700951010     C                   ELSE
039800070905     C     LIN           IFGT      58
039900951010     C                   WRITE     TESGEN
040000951010     C                   WRITE     TESCLI
040100951010     C                   WRITE     TESTOT
040200951010     C                   WRITE     TESMOT
040300951010     C                   END
040400951010     C                   END
040500951010     C                   WRITE     DETTAG
040600951010     C                   END
040700951010     C                   ADD       1             X
040800951010     C                   END
040900951010      *  Stampo totale lna
041000070704     C     LIN           IFGT      57
041100951010     C                   WRITE     TESGEN
041200951010     C                   WRITE     TESCLI
041300951010     C                   WRITE     TESTOT
041400951010     C                   END
041500951010     C                   Z-ADD     WTTSPE        TOTSPE
041600951010     C                   Z-ADD     WTTGIA        TOTGIA
041700951010     C                   Z-ADD     WTTRES        TOTRES
041800951010     C                   Z-ADD     WTTGAP        TOTGAP
041900041006     C                   Z-ADD     wttCSP        TOTCSP
042000041006     C                   Z-ADD     wttNSP        TOTNSP
042100041006?     *  Calcolo Percentuali
042200041006     c                   exsr      CALC_PERC
042300041006      *
042400041006     C                   ENDSR
042500041006?     *--------------------------------------------------------------*
042600041006?     *  Calcolo Percentuali                                         *
042700041006?     *--------------------------------------------------------------*
042800041006     C     CALC_PERC     BEGSR
042900041006      *
043000041006      *  Calcolo percentuali
043100041006     C     TOTGIA        MULT      100           WCOM14           14 0
043200041006     C     WCOM14        DIV(H)    TOTSPE        SPEGIA
043300041006      *
043400041006     C     TOTRES        MULT      100           WCOM14           14 0
043500041006     C     TOTGIA        IFNE      0
043600041006     C     WCOM14        DIV(H)    TOTGIA        GIARES
043700041006     C                   ELSE
043800041006     C                   Z-ADD     0             GIARES
043900041006     C                   END
044000041006      *
044100041006     C     totGAP        MULT      100           Wcom14           14 0
044200041006     C     totGIA        IFNE      0
044300041006     C     Wcom14        DIV(H)    totGIA        NCHGIA
044400041006     C                   ELSE
044500041006     C                   Z-ADD     0             NCHGIA
044600041006     C                   END
044700041006      *
044800041006     C     totCSP        MULT      100           Wcom14           14 0
044900041006     C     totGIA        IFNE      0
045000041006     C     Wcom14        DIV(H)    totGIA        CSPGIA
045100041006     C                   ELSE
045200041006     C                   Z-ADD     0             CSPGIA
045300041006     C                   END
045400041006      *
045500041006      *
045600041006     C     totNSP        MULT      100           Wcom14           14 0
045700041006     C     totGIA        IFNE      0
045800041006     C     Wcom14        DIV(H)    totGIA        NSPGIA
045900041006     C                   ELSE
046000041006     C                   Z-ADD     0             NSPGIA
046100041006     C                   END
046200041006      *
046300041006     C                   WRITE     TOTLNA
046400951010      *
046500951010     C                   ENDSR
046600951003?     *--------------------------------------------------------------*
046700951003?     *  Operazioni iniziali                                         *
046800951003?     *--------------------------------------------------------------*
046900951003     C     *INZSR        BEGSR
047000951003      *
047100951003     C     *ENTRY        PLIST
047200951003     C                   PARM                    KPJBA
047300951003     C                   MOVEL     KPJBU         FNLG48
047400951003     C* Richiamo XPARUT
047500951003     C                   Z-ADD     1             CODUT
047600951003     C                   CALL      'X§PARUT'
047700951003     C                   PARM                    UTEDSE
047800951003     C                   MOVEL     RAGUT         RSUT
047900951003     C                   MOVEL     REC80         CNCR80
048000951003     C                   MOVEL     SIMFEL        STPFEL
048100020719      *
048200020719      * reperisce le linee di partenza dell'area
048300020719     c                   clear                   trul06ds
048400021010     C                   MOVEL     kpjbu         KPJBUs
048500020719     c                   eval      d06cod = '£1'
048600020719     c                   movel     simfel        d06key
048700020719     c                   movel     trul06ds      kpjbu
048800020719     c                   call      'TRUL06R'
048900020719     c                   parm                    kpjba
049000020719     c                   movel     kpjbu         trul06ds
049100021010     C                   MOVEL     kpjbus        KPJBU
049200020719      *
049300951003     C*--- Ricerca Capoconti -----------------------------------------*
049400951003     C                   DO        50            X
049500951003     C                   MOVE      TCU(X)        TCUDS
049600951003     C     F56           IFEQ      'CG'
049700951003     C     F34           ANDEQ     '01'
049800951003     C                   Z-ADD     KCU(X)        CLIITA            4 0
049900951003     C                   END
050000951003     C                   END
050100021011      *
050200951003      *  Definisco chiavi di accesso ai file
050300021011     C     Key_set       KLIST
050400021011     C                   KFLD                    xlnp
050500021011     C                   KFLD                    xlna
050600021011     C                   KFLD                    da_aas
050700021011     C                   KFLD                    da_mgs
050800021011      *
050900021011     C     Key_Lna       KLIST
051000021011     C                   KFLD                    xlnp
051100021011     C                   KFLD                    xlna
051200021011      *
051300951003     C     KGCP          KLIST
051400951003     C                   KFLD                    KAAS
051500951003     C                   KFLD                    KLNP
051600951003     C                   KFLD                    KNRS
051700951003     C                   KFLD                    KNSP
051800951003     C                   KFLD                    KFRG
051900021011      *
052000951003     C     KTAB          KLIST
052100951003     C                   KFLD                    KKUT
052200951003     C                   KFLD                    KCOD
052300021011      *
052400951003     C     KACO          KLIST
052500951003     C                   KFLD                    KKUT
052600951003     C                   KFLD                    KKCC
052700951003     C                   KFLD                    KKSC
052800021011      *
052900951003      *  Carico tabella codici eventi
053000070704     c                   clear                   mult
053100070704     c                   clear                   tot
053200951003     C                   Z-ADD     0             X
053300951003     C                   Z-ADD     1             KKUT
053400951003     C                   MOVEL     '2A'          KCOD
053500951003     C                   OPEN      TABEL00F
053600951003     C     KTAB          CHAIN     TABEL00F                           31
053700951003     C     *IN31         DOWEQ     '0'
053800090409     C     X             ANDLT     200
053900951003     C                   ADD       1             X
054000951003     C                   MOVEL     TBLKEY        EVE(X)
054100951003     C     X             OCCUR     MULT
054200951003     C                   MOVEL     TBLUNI        WDESEV
054300951010     C     X             OCCUR     TOT
054400951010     C                   MOVEL     TBLUNI        WTDESE
054500951003     C     KTAB          READE     TABEL00F                               31
054600951003     C                   END
054700951003     C                   CLOSE     TABEL00F
054800951003     C     *IN31         IFEQ      '0'
054900951003     C                   EXCEPT    TESERR
055000951003     C                   EXCEPT    DETERR
055100951003     C                   GOTO      FINE
055200951003     C                   END
055300021011      *
055400960209     C                   SETOFF                                       010203
055500021011      *
055600960209      *  Controllo se devo decodificare il cliente
055700960209     C     D48MOD        IFEQ      'C'
055800960209     C                   SETON                                        01
055900960209      *  Decodifico cod.cliente
056000951003     C                   OPEN      CNACO00F
056100951003     C                   Z-ADD     CLIITA        KKCC
056200951003     C                   Z-ADD     KSC(1)        KKSC
056300951003     C                   MOVEL     *BLANKS       ACORAG
056400951003     C     KACO          CHAIN     CNACO00F                           31
056500951003     C                   MOVEL     ACORAG        DESCLI
056600960212     C                   ELSE
056700960209      *  Controllo se devo decodificare lnp o area
056800960209     C     KSC(2)        IFNE      0
056900960209     C                   SETON                                        03
057000960209     C                   Z-ADD     SIMFEL        ORGFIL
057100960209     C                   ELSE
057200960212     C                   SETON                                        02
057300960209     C                   Z-ADD     KSC(1)        ORGFIL
057400960209     C                   END
057500960209     C                   MOVEL     *BLANKS       ORGDES
057600960209     C     ORGFIL        CHAIN     AZORG01L                           31
057700960209     C                   MOVEL     ORGDES        DESCLI
057800960209     C                   END
057900960209      *
058000951003      *  Giro le date spedizioni
058100951003     C                   Z-ADD     D48DSD        G02INV
058200951003     C                   MOVE      '3'           G02ERR
058300951003     C                   CALL      'XSRDA8'
058400951003     C                   PARM                    WLBDA8
058500951003     C                   Z-ADD     G02DAT        DATADA
058600951003     C                   Z-ADD     D48DSA        G02INV
058700951003     C                   MOVE      '3'           G02ERR
058800951003     C                   CALL      'XSRDA8'
058900951003     C                   PARM                    WLBDA8
059000951003     C                   Z-ADD     G02DAT        DATAAL
059100951003     C                   TIME                    TIMES             6 0
059200021011      *
059300951003     C                   Z-ADD     D48DSD        KMGS
059400951003     C                   MOVEL     D48DSD        KAAS
059500021011      *
059600021011     C                   Z-ADD     D48DSD        da_mgs
059700021011     C                   MOVEL     D48DSD        da_aas
059800021011      *
059900041006     C                   MOVEL     *ALL'-'       CMP198
060000021011      *
060100951010      *  Azzero DS multipla totali
060200951010     C                   Z-ADD     1             X                 4 0
060300090409     C     X             DOWLE     200
060400951010     C     EVE(X)        ANDNE     *BLANKS
060500951010     C     X             OCCUR     TOT
060600951010     C                   Z-ADD     0             WTGIAC
060700951010     C                   Z-ADD     0             WTOGAP
060800041006     C                   Z-ADD     0             Wtgcsp
060900041006     C                   Z-ADD     0             Wtgnsp
061000951010     C                   ADD       1             X
061100951010     C                   END
061200951010     C                   Z-ADD     0             WTTSPE
061300951010     C                   Z-ADD     0             WTTGIA
061400951010     C                   Z-ADD     0             WTTRES
061500951010     C                   Z-ADD     0             WTTGAP
061600041006     C                   Z-ADD     0             WTTcsp
061700041006     C                   Z-ADD     0             WTTnsp
061800021011      *  Eseguo stampa prima pagina
061900021011     C                   EXSR      STPINI
062000951010      *
062100951003     C                   ENDSR
062200021011?     *--------------------------------------------------------------*
062300021011?     *  STPINI: Stampa prima pagina                                 *
062400021011?     *--------------------------------------------------------------*
062500021011     C     STPINI        BEGSR
062600021011      *
062700021011     c                   clear                   y                 3 0
062800021011      *
062900021011     C                   WRITE     PRIPAG
063000021011     C                   WRITE     DETSPA
063100021011      *
063200021011     C                   DO        20            X
063300021011      *
063400021011     C     KSC(X)        IFNE      0
063500021011     C                   Z-ADD     KSC(X)        STPKSC
063600021011     c                   add       1             y
063700021011      *  x cliente
063800021011     C     D48MOD        IFEQ      'C'
063900021011     C                   Z-ADD     CLIITA        KKCC
064000021011     C                   Z-ADD     KSC(X)        KKSC
064100021011     C                   MOVEL     *BLANKS       ACORAG
064200021011     C     KACO          CHAIN     CNACO00F                           31
064300021011     C                   MOVEL     ACORAG        STPDES
064400021011     C                   ELSE
064500021011      *  x LNP
064600021011     C                   Z-ADD     KSC(X)        ORGFIL
064700021011     C                   MOVEL     *BLANKS       ORGDES
064800021011     C     ORGFIL        CHAIN     AZORG01L                           31
064900021011     C                   MOVEL     ORGDES        STPDES
065000021011     C                   END
065100021011      *
065200021011     C                   WRITE     DETCLI
065300021011     C                   WRITE     DETSPA
065400021011      *
065500021011     C                   END
065600021011     C                   END
065700021011     C*
065800021011     C                   WRITE     FINPAG
065900021011     C                   CLOSE     CNACO00F
066000021011     C*
066100021011     C                   ENDSR
066200951003      *--------------------------------------------------------------*
066300951003     O*  STAMPA ERRORI !!!!!!!!
066400951003      *--------------------------------------------------------------*
066500951003     OSYSPRT    E            TESERR           01
066600951003     O                       STT(1)              66
066700951003     O                       STT(2)             132
066800951003     O                       RSUT                20
066900951003     O                       UDATE              118 '  /  /  '
067000951003     O                       PAGE          Z    132
067100951003     O          E            TESERR           03
067200951003     O                       TIMES              118 '  .  .  '
067300951003     O          E            TESERR           06
067400951003     O                                           24 'SI RACCOMANDA DI CONSEGN'
067500951003     O                                           48 'ARE QUESTA STAMPA ALL''U'
067600951003     O                                           72 'FFICIO  C.E.D.!!!!!!    '
067700951003     O          E            TESERR      0  2
067800951003     O                                           24 'SI RACCOMANDA DI CONSEGN'
067900951003     O                                           48 'ARE QUESTA STAMPA ALL''U'
068000951003     O                                           72 'FFICIO  C.E.D.!!!!!!    '
068100951003     O          E            DETERR         1
068200951003     O                                           24 'NON CARICATI TUTTI GLI E'
068300951003     O                                           48 'LEMENTI DELLA TABELLA 2A'
068400951003      *--------------------------------------------------------------*
068500990628** STT
068600951003                                 ***  S T A M P A     G I A C E N
068700070725Z E      P E R     C L I E N T E  ***  FNLG53R              PAG.
