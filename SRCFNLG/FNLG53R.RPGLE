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
005200150710      *
005300150710     d  linpt          s              3  0 dim(200)
005400021011      *
005500951003     D KPJBA         E DS
005600150710     D FNLG52ds      E DS
005700021011     D  Ksc                   17    156  0 dim(20)
005800021011      *
005900021010     D kpjbus          S                   LIKE(kpjbu)
006000021011      *
006100951003      *  Ds multipla x memorizzazione totali statistica
006200090409     D MULT            DS           123    OCCURS(200)
006300951003     D  WDESEV                 1     75
006400951003     D  WGCSPE                76     87  0
006500951003     D  WTOTAP                88     99  0
006600041006     D  WGCcsp               100    111  0
006700041006     D  WGCnsp               112    123  0
006800951010      *  Ds multipla x TOTALI
006900090409     D TOT             DS           123    OCCURS(200)
007000951010     D  WTDESE                 1     75
007100951010     D  WTGIAC                76     87  0
007200951010     D  WTOGAP                88     99  0
007300041006     D  Wtgcsp               100    111  0
007400041006     D  Wtgnsp               112    123  0
007500951003     D WLBDA8          DS
007600951003     D  G02DAT                 1      8  0
007700951003     D  G02INV                 9     16  0
007800951003     D  G02ERR                17     17
007900951003     D  G02TGI                18     22  0
008000951003     D                SDS
008100951003     D  NOMPGM                 1     10
008200021011      *
008300021011      *  Definisco variabili
008400070703     d wlna                                like(gcplna)
008500070703     d klna                                like(gcplna)
008600070703     d kaas                                like(gcpaas)
008700070703     d da_aas                              like(gcpaas)
008800070703     d da_mgs                              like(gklmgs)
008900070703     d kmgs                                like(gklmgs)
009000021011     d klnp                                like(gcplnp)
009100021011     d knrs                                like(gcpnrs)
009200021011     d knsp                                like(gcpnsp)
009300021011     d kfrg                                like(gcpfrg)
009400021011     d kkut                                like(tblkut)
009500021011     d kcod                                like(tblcod)
009600021011     d kkcc                                like(acokcc)
009700021011     d kksc                                like(acoksc)
009800041006      *
009900021011     d wtotsp                              like(totspe)
010000021011     d wtotgc                              like(totgia)
010100021011     d wtotrs                              like(totres)
010200021011     d wtotga                              like(totgap)
010300041006     d wtotcs                              like(totcsp)
010400041006     d wtotns                              like(totnsp)
010500041006      *
010600021011     d wttspe                              like(totspe)
010700021011     d wttgia                              like(totgia)
010800021011     d wttres                              like(totres)
010900021011     d wttgap                              like(totgap)
011000041006     d wttcsp                              like(totcsp)
011100041006     d wttnsp                              like(totnsp)
011200041006      *
011300041006     d tspese          S             11  3
011400951003?     *--------------------------------------------------------------*
011500951003?     *  CICLO PRINCIPALE                                            *
011600951003?     *--------------------------------------------------------------*
011700021011      *  Esegeue un ciclo per LNA x filiali
011800021011     c     *start        setll     azorg01l
011900021011     c                   read      azorg01l
012000021011      *
012100021011     c                   dow       not %eof(azorg01l)
012200021011      *  solo quelli validi
012300021011     c                   if        orgfva = *blank and
012400021011     c                             (orgfag = 'F' or orgfag = 'A')
012500021011      *  Inzializzo dati x lna
012600021011     C                   EXSR      INZLNA
012700021011      *
012800021011      *  Esegeue un ciclo per LNP da 1 a 30
012900150710     c******             do        30            lnp30             3 0
013000150710      *  Esegeue un ciclo per LNP da 1 a 200 per nuova skiera
013100150710     c                   do        200           lnp30             3 0
013200021011      *  quando ha esaurito le LNP esce da questo ciclo x leggere una
013300021011      *  nuova LNA
013400150710     c                   if        linpt(lnp30) = 0
013500021011     c                   leave
013600021011     c                   endIF
013700021011      *
013800021011      * elaborazione x LNP
013900021011      *  seleziona solo le linee dell'area richieste
014000021011      * posizionandosi ottimizzando il giro.
014100021011      *  altrimenti
014200021011      * elaborazione x Cliente
014300021011      *  deve leggerle tutte scartando x cliente.
014400150710     c                   if        d52mod <> 'C'
014500150710     c                   z-add     linpt(lnp30)  wlinp             7 0
014600021011     c     wlinp         lookup    Ksc                                    31
014700021011     c  n31              goto      lnp_new
014800021011     c                   end
014900021011      *
015000021011      * posizionamento iniziale di lettura
015100021011      *  x chiave completa con data inizio
015200021011      *    e lettura x LNP/LNA
015300150710     c                   z-add     linpt(lnp30)  xlnp              3 0
015400070703     C     Key_set       Setll     wgcpkl0f
015500070703     C     Key_Lna       reade     wgcpkl0f                               30
015600021011      *  Legge fino alla data
015700021011     c     *in30         doweq     *Off
015800021011      *
015900021011      *     Dettaglio di calcolo x totali da stampare
016000021011     C                   EXSR      GESLNA
016100021011      *
016200070703     C     Key_Lna       reade     wgcpkl0f                               30
016300021011      * Controlla limite data superiore
016400021011     c                   enddo
016500021011      *
016600021011     c     lnp_new       tag
016700021011      * lnpx
016800021011     C                   ENDdo
016900021011      *
017000021011      *  Totali per linea di arrivo
017100021011     C                   EXSR      TOTLIN
017200021011     c                   endIF
017300021011      * lnax
017400021011     c                   read      azorg01l
017500021011     C                   ENDdo
017600021011      *
017700951010      *  Se ho trovato almeno una sped. per il cliente stampo tot.gener.
017800951010     C     WTTSPE        IFNE      0
017900951010     C                   EXSR      TOTGEN
018000951010     C                   END
018100951003      *  Fine Lavoro
018200951003     C     FINE          TAG
018300951003     C                   SETON                                        LR
018400951003?     *--------------------------------------------------------------*
018500951003?     *  Operazione di inizializzazione totali x lna                 *
018600951003?     *--------------------------------------------------------------*
018700951003     C     INZLNA        BEGSR
018800951003      *
018900951003     C                   Z-ADD     1             X                 4 0
019000090409     C     X             DOWLE     200
019100951003     C     EVE(X)        ANDNE     *BLANKS
019200951003     C     X             OCCUR     MULT
019300951003     C                   Z-ADD     0             WGCSPE
019400951003     C                   Z-ADD     0             WTOTAP
019500041006     C                   Z-ADD     0             wgcCSP
019600041006     C                   Z-ADD     0             wgcNSP
019700951003     C                   ADD       1             X
019800951003     C                   END
019900021011      *
020000951003     C                   Z-ADD     0             WTOTSP
020100951003     C                   Z-ADD     0             WTOTGC
020200951003     C                   Z-ADD     0             WTOTRS
020300951003     C                   Z-ADD     0             WTOTGA
020400041006     C                   Z-ADD     0             WTOTcs
020500041006     C                   Z-ADD     0             WTOTns
020600951003      *  Decodifico lna
020700021011     C                   Z-ADD     ORGFIL        xlna              3 0
020800951003     C                   MOVEL     ORGDES        DESLNA
020900951003     C                   Z-ADD     ORGFIL        CODLNA
021000951003      *
021100951003     C                   ENDSR
021200951003?     *--------------------------------------------------------------*
021300951003?     *  Gestione singola lna                                        *
021400951003?     *--------------------------------------------------------------*
021500951003     C     GESLNA        BEGSR
021600070831      *  Controllo se devo testare cliente o lnp
021700150710     C     d52MOD        IFEQ      'C'
021800070831      *  Controllo se cliente letto è da considerare
021900070831     C     gklccm        IFNE      0
022000070831     C     gklccm        LOOKUP    KSC                                    31
022100070831     C                   END
022200070831     C*
022300070831     C                   ELSE
022400070831      *  Controllo se lnp letta è da considerare
022500070831     C                   Z-ADD     gklLNP        WLNP              7 0
022600070831     C     WLNP          LOOKUP    KSC                                    31
022700070831     C                   END
022800070831     C*
022900070831     C     *IN31         IFEQ      '1'
023000951003      *
023100951003     C                   ADD       1             WTOTSP
023200951003      *  Controllo se cliente ha avuto giacenza
023300070703     C                   Z-ADD     gklAAS        KAAS
023400070703     C                   Z-ADD     gklLNP        KLNP
023500070703     C                   Z-ADD     gklNRS        KNRS
023600070703     C                   Z-ADD     gklNSP        KNSP
023700951003     C                   Z-ADD     0             KFRG
023800070703     C     KGCP          CHAIN     tigcp51L                           32
023900951003     C     *IN32         IFEQ      '0'
024000951003     C                   ADD       1             WTOTGC
024100951003     C     GCPDCG        IFEQ      0
024200951003     C                   ADD       1             WTOTGA
024300951009     C                   END
024400951003     C     GCPDIS        IFEQ      'R'
024500951003     C                   ADD       1             WTOTRS
024600951003     C                   END
024700041006      **  giacenza chiusa
024800041006     C                   If        gcpDCG > 0 and gcpFAS >= 50
024900041006      ** controllo se ha spese
025000041006     c                   eval      tspese = gcpSGS + gcpSGR + gcpSGD + gcpSGA
025100041006      *  se ci sono degli importi ed è stata creata la bolla allora
025200041006      *   ci sono delle spese
025300041006     c                   if        tspese > 0 and gcpNBW > 0
025400041006     C                   ADD       1             WTOTcs
025500041006     C                   else
025600041006     C                   ADD       1             WTOTns
025700041006     C                   end
025800041006     C                   end
025900041006      **
026000951003     C                   END
026100070831     C                   END
026200960209     C*
026300951003      *  Se cliente da considerare e trovato giacenza incremento
026400951003      *  totali motivi giacenza
026500070704     C     *IN31         IFEQ      '1'
026600070704     C     *IN32         andeq     '0'
026700951003     C                   Z-ADD     1             X
026800951003     C     GCPCMC        LOOKUP    EVE(X)                                 31
026900951005     C     X             OCCUR     MULT
027000951003     C     *IN31         IFEQ      '1'
027100951003     C                   ADD       1             WGCSPE
027200951003     C     GCPDCG        IFEQ      0
027300951003     C                   ADD       1             WTOTAP
027400951003     C                   END
027500041006      **  giacenza chiusa
027600041006     C                   If        gcpDCG > 0 and gcpFAS >= 50
027700041006      ** controllo se ha spese
027800041006     c                   eval      tspese = gcpSGS + gcpSGR + gcpSGD + gcpSGA
027900041006      *  se ci sono degli importi ed è stata creata la bolla allora
028000041006      *   ci sono delle spese
028100041006     c                   if        tspese > 0 and gcpNBW > 0
028200041006     C                   ADD       1             wgcCSP
028300041006     C                   else
028400041006     C                   ADD       1             wgcNSP
028500041006     C                   end
028600041006     C                   end
028700041006      **
028800951003     C                   END
028900951003     C                   END
029000951003     C*
029100951003     C                   ENDSR
029200951003?     *--------------------------------------------------------------*
029300951003?     *  Operazione di inizializzazione totali x lna                 *
029400951003?     *--------------------------------------------------------------*
029500951003     C     TOTLIN        BEGSR
029600951003      *
029700951003     C                   MOVEL     'N'           WSTMOT            1
029800951003      *  Scrivo totali x cliente/lna
029900951003     C     WTOTSP        IFNE      0
030000951003     C     WPRPAG        IFNE      'N'
030100070704     C     LIN           ORGT      58
030200951003     C                   MOVEL     'N'           WPRPAG            1
030300951003     C                   WRITE     TESGEN
030400951003     C                   WRITE     TESCLI
030500951003     C                   END
030600951003     C                   WRITE     TESLNA
030700951003     C*
030800951003     C                   Z-ADD     1             X
030900090409     C     X             DOWLE     200
031000951003     C     EVE(X)        ANDNE     *BLANKS
031100951003     C     X             OCCUR     MULT
031200951003     C     WGCSPE        IFNE      *ZEROS
031300951003     C                   Z-ADD     WGCSPE        GIAEVE
031400951003     C                   Z-ADD     WTOTAP        APEEVE
031500041006     C                   Z-ADD     wgcCSP        conspe
031600041006     C                   Z-ADD     wgcNSP        senspe
031700951010     C     X             OCCUR     TOT
031800951010     C                   ADD       WGCSPE        WTGIAC
031900951010     C                   ADD       WTOTAP        WTOGAP
032000041006     C                   ADD       wgcCSP        wtgCSP
032100041006     C                   ADD       wgcNSP        wtgNSP
032200951003     C                   MOVEL     EVE(X)        CODEVE
032300951003     C                   MOVEL     WDESEV        DESEVE
032400951003     C     WSTMOT        IFEQ      'N'
032500070704     C     LIN           IFGT      55
032600951005     C                   WRITE     TESGEN
032700951005     C                   WRITE     TESCLI
032800951005     C                   WRITE     TESLNA
032900951005     C                   END
033000951005     C                   MOVEL     'S'           WSTMOT            1
033100951005     C                   WRITE     TESMOT
033200951005     C                   ELSE
033300070704     C     LIN           IFGT      58
033400951005     C                   WRITE     TESGEN
033500951005     C                   WRITE     TESCLI
033600951005     C                   WRITE     TESLNA
033700951005     C                   WRITE     TESMOT
033800951005     C                   END
033900951003     C                   END
034000951003     C                   WRITE     DETTAG
034100951003     C                   END
034200951003     C                   ADD       1             X
034300951003     C                   END
034400951003      *  Stampo totale lna
034500070704     C     LIN           IFGT      57
034600951003     C                   WRITE     TESGEN
034700951003     C                   WRITE     TESCLI
034800951003     C                   WRITE     TESLNA
034900951003     C                   END
035000951003     C                   Z-ADD     WTOTSP        TOTSPE
035100951003     C                   Z-ADD     WTOTGC        TOTGIA
035200951003     C                   Z-ADD     WTOTRS        TOTRES
035300951003     C                   Z-ADD     WTOTGA        TOTGAP
035400041006     C                   Z-ADD     WTOTcs        totCSP
035500041006     C                   Z-ADD     WTOTns        totNSP
035600951010     C                   ADD       WTOTSP        WTTSPE
035700951010     C                   ADD       WTOTGC        WTTGIA
035800951010     C                   ADD       WTOTRS        WTTRES
035900951010     C                   ADD       WTOTGA        WTTGAP
036000041006     C                   add       WTOTcs        wttCSP
036100041006     C                   add       WTOTns        wttNSP
036200041006      *  Calcolo percentuali
036300041006     c                   exsr      CALC_PERC
036400951003     C                   END
036500951003      *
036600951003     C                   ENDSR
036700951010?     *--------------------------------------------------------------*
036800951010?     *  Operazione di stampa totali generali                        *
036900951010?     *--------------------------------------------------------------*
037000951010     C     TOTGEN        BEGSR
037100951010      *
037200951010     C                   MOVEL     'N'           WSTMOT            1
037300951010      *  Scrivo totali x cliente/lna
037400951010     C     WPRPAG        IFNE      'N'
037500070705     C     LIN           ORGT      55
037600951010     C                   MOVEL     'N'           WPRPAG            1
037700951010     C                   WRITE     TESGEN
037800951010     C                   WRITE     TESCLI
037900951010     C                   END
038000951010     C                   WRITE     TESTOT
038100951010     C*
038200951010     C                   Z-ADD     1             X
038300090409     C     X             DOWLE     200
038400951010     C     EVE(X)        ANDNE     *BLANKS
038500951010     C     X             OCCUR     TOT
038600951010     C     WTGIAC        IFNE      *ZEROS
038700951010     C                   Z-ADD     WTGIAC        GIAEVE
038800951010     C                   Z-ADD     WTOGAP        APEEVE
038900041006     C                   Z-ADD     wtgCSP        conSPE
039000041006     C                   Z-ADD     wtgNSP        senSPE
039100951010     C                   MOVEL     EVE(X)        CODEVE
039200951010     C                   MOVEL     WTDESE        DESEVE
039300951010     C     WSTMOT        IFEQ      'N'
039400070704     C     LIN           IFGT      55
039500951010     C                   WRITE     TESGEN
039600951010     C                   WRITE     TESCLI
039700951010     C                   WRITE     TESTOT
039800951010     C                   END
039900951010     C                   MOVEL     'S'           WSTMOT            1
040000951010     C                   WRITE     TESMOT
040100951010     C                   ELSE
040200070905     C     LIN           IFGT      58
040300951010     C                   WRITE     TESGEN
040400951010     C                   WRITE     TESCLI
040500951010     C                   WRITE     TESTOT
040600951010     C                   WRITE     TESMOT
040700951010     C                   END
040800951010     C                   END
040900951010     C                   WRITE     DETTAG
041000951010     C                   END
041100951010     C                   ADD       1             X
041200951010     C                   END
041300951010      *  Stampo totale lna
041400070704     C     LIN           IFGT      57
041500951010     C                   WRITE     TESGEN
041600951010     C                   WRITE     TESCLI
041700951010     C                   WRITE     TESTOT
041800951010     C                   END
041900951010     C                   Z-ADD     WTTSPE        TOTSPE
042000951010     C                   Z-ADD     WTTGIA        TOTGIA
042100951010     C                   Z-ADD     WTTRES        TOTRES
042200951010     C                   Z-ADD     WTTGAP        TOTGAP
042300041006     C                   Z-ADD     wttCSP        TOTCSP
042400041006     C                   Z-ADD     wttNSP        TOTNSP
042500041006?     *  Calcolo Percentuali
042600041006     c                   exsr      CALC_PERC
042700041006      *
042800041006     C                   ENDSR
042900041006?     *--------------------------------------------------------------*
043000041006?     *  Calcolo Percentuali                                         *
043100041006?     *--------------------------------------------------------------*
043200041006     C     CALC_PERC     BEGSR
043300041006      *
043400041006      *  Calcolo percentuali
043500041006     C     TOTGIA        MULT      100           WCOM14           14 0
043600041006     C     WCOM14        DIV(H)    TOTSPE        SPEGIA
043700041006      *
043800041006     C     TOTRES        MULT      100           WCOM14           14 0
043900041006     C     TOTGIA        IFNE      0
044000041006     C     WCOM14        DIV(H)    TOTGIA        GIARES
044100041006     C                   ELSE
044200041006     C                   Z-ADD     0             GIARES
044300041006     C                   END
044400041006      *
044500041006     C     totGAP        MULT      100           Wcom14           14 0
044600041006     C     totGIA        IFNE      0
044700041006     C     Wcom14        DIV(H)    totGIA        NCHGIA
044800041006     C                   ELSE
044900041006     C                   Z-ADD     0             NCHGIA
045000041006     C                   END
045100041006      *
045200041006     C     totCSP        MULT      100           Wcom14           14 0
045300041006     C     totGIA        IFNE      0
045400041006     C     Wcom14        DIV(H)    totGIA        CSPGIA
045500041006     C                   ELSE
045600041006     C                   Z-ADD     0             CSPGIA
045700041006     C                   END
045800041006      *
045900041006      *
046000041006     C     totNSP        MULT      100           Wcom14           14 0
046100041006     C     totGIA        IFNE      0
046200041006     C     Wcom14        DIV(H)    totGIA        NSPGIA
046300041006     C                   ELSE
046400041006     C                   Z-ADD     0             NSPGIA
046500041006     C                   END
046600041006      *
046700041006     C                   WRITE     TOTLNA
046800951010      *
046900951010     C                   ENDSR
047000951003?     *--------------------------------------------------------------*
047100951003?     *  Operazioni iniziali                                         *
047200951003?     *--------------------------------------------------------------*
047300951003     C     *INZSR        BEGSR
047400951003      *
047500951003     C     *ENTRY        PLIST
047600951003     C                   PARM                    KPJBA
047700150710     C                   MOVEL     KPJBU         FNLG52ds
047800951003     C* Richiamo XPARUT
047900951003     C                   Z-ADD     1             CODUT
048000951003     C                   CALL      'X§PARUT'
048100951003     C                   PARM                    UTEDSE
048200951003     C                   MOVEL     RAGUT         RSUT
048300951003     C                   MOVEL     REC80         CNCR80
048400951003     C                   MOVEL     SIMFEL        STPFEL
048500020719      *
048600020719      * reperisce le linee di partenza dell'area
048700020719     c                   clear                   trul06ds
048800021010     C                   MOVEL     kpjbu         KPJBUs
048900020719     c                   eval      d06cod = '£1'
049000020719     c                   movel     simfel        d06key
049100020719     c                   movel     trul06ds      kpjbu
049200020719     c                   call      'TRUL06R'
049300020719     c                   parm                    kpjba
049400020719     c                   movel     kpjbu         trul06ds
049500021010     C                   MOVEL     kpjbus        KPJBU
049600150710      *imposta skiera filiale partenza se solo terminal da £1 altrimenti azorg
049700150710     c                   if        d52ter = 'S'
049800150710     c                   do        30            x
049900150710     c                   z-add     linp(x)       linpt(x)
050000150710     c                   enddo
050100150710     c                   else
050200150710     c                   z-add     0             x
050300150710     c     *start        setll     azorg01l
050400150710     c                   do        *hival
050500150710     c                   read      azorg01l
050600150710      *
050700150710     c                   if        %eof(azorg01l) or x = 200
050800150710     c                   leave
050900150710     c                   endif
051000150710      *  solo quelli validi
051100150710     c                   if        orgfva = *blank and
051200150710     c                             (orgfag = 'F' or orgfag = 'A')
051300150710     c                   add       1             x
051400150710     c                   z-add     orgfil        linpt(x)
051500150710     c                   endif
051600150710     c                   enddo
051700150710     c                   endif
051800020719      *
051900951003     C*--- Ricerca Capoconti -----------------------------------------*
052000951003     C                   DO        50            X
052100951003     C                   MOVE      TCU(X)        TCUDS
052200951003     C     F56           IFEQ      'CG'
052300951003     C     F34           ANDEQ     '01'
052400951003     C                   Z-ADD     KCU(X)        CLIITA            4 0
052500951003     C                   END
052600951003     C                   END
052700021011      *
052800951003      *  Definisco chiavi di accesso ai file
052900021011     C     Key_set       KLIST
053000021011     C                   KFLD                    xlnp
053100021011     C                   KFLD                    xlna
053200021011     C                   KFLD                    da_aas
053300021011     C                   KFLD                    da_mgs
053400021011      *
053500021011     C     Key_Lna       KLIST
053600021011     C                   KFLD                    xlnp
053700021011     C                   KFLD                    xlna
053800021011      *
053900951003     C     KGCP          KLIST
054000951003     C                   KFLD                    KAAS
054100951003     C                   KFLD                    KLNP
054200951003     C                   KFLD                    KNRS
054300951003     C                   KFLD                    KNSP
054400951003     C                   KFLD                    KFRG
054500021011      *
054600951003     C     KTAB          KLIST
054700951003     C                   KFLD                    KKUT
054800951003     C                   KFLD                    KCOD
054900021011      *
055000951003     C     KACO          KLIST
055100951003     C                   KFLD                    KKUT
055200951003     C                   KFLD                    KKCC
055300951003     C                   KFLD                    KKSC
055400021011      *
055500951003      *  Carico tabella codici eventi
055600070704     c                   clear                   mult
055700070704     c                   clear                   tot
055800951003     C                   Z-ADD     0             X
055900951003     C                   Z-ADD     1             KKUT
056000951003     C                   MOVEL     '2A'          KCOD
056100951003     C                   OPEN      TABEL00F
056200951003     C     KTAB          CHAIN     TABEL00F                           31
056300951003     C     *IN31         DOWEQ     '0'
056400090409     C     X             ANDLT     200
056500951003     C                   ADD       1             X
056600951003     C                   MOVEL     TBLKEY        EVE(X)
056700951003     C     X             OCCUR     MULT
056800951003     C                   MOVEL     TBLUNI        WDESEV
056900951010     C     X             OCCUR     TOT
057000951010     C                   MOVEL     TBLUNI        WTDESE
057100951003     C     KTAB          READE     TABEL00F                               31
057200951003     C                   END
057300951003     C                   CLOSE     TABEL00F
057400951003     C     *IN31         IFEQ      '0'
057500951003     C                   EXCEPT    TESERR
057600951003     C                   EXCEPT    DETERR
057700951003     C                   GOTO      FINE
057800951003     C                   END
057900021011      *
058000960209     C                   SETOFF                                       010203
058100021011      *
058200960209      *  Controllo se devo decodificare il cliente
058300150710     C     d52MOD        IFEQ      'C'
058400960209     C                   SETON                                        01
058500960209      *  Decodifico cod.cliente
058600951003     C                   OPEN      CNACO00F
058700951003     C                   Z-ADD     CLIITA        KKCC
058800951003     C                   Z-ADD     KSC(1)        KKSC
058900951003     C                   MOVEL     *BLANKS       ACORAG
059000951003     C     KACO          CHAIN     CNACO00F                           31
059100951003     C                   MOVEL     ACORAG        DESCLI
059200960212     C                   ELSE
059300960209      *  Controllo se devo decodificare lnp o area
059400960209     C     KSC(2)        IFNE      0
059500960209     C                   SETON                                        03
059600960209     C                   Z-ADD     SIMFEL        ORGFIL
059700960209     C                   ELSE
059800960212     C                   SETON                                        02
059900960209     C                   Z-ADD     KSC(1)        ORGFIL
060000960209     C                   END
060100960209     C                   MOVEL     *BLANKS       ORGDES
060200960209     C     ORGFIL        CHAIN     AZORG01L                           31
060300960209     C                   MOVEL     ORGDES        DESCLI
060400960209     C                   END
060500960209      *
060600951003      *  Giro le date spedizioni
060700150710     C                   Z-ADD     d52DSD        G02INV
060800951003     C                   MOVE      '3'           G02ERR
060900951003     C                   CALL      'XSRDA8'
061000951003     C                   PARM                    WLBDA8
061100951003     C                   Z-ADD     G02DAT        DATADA
061200150710     C                   Z-ADD     d52DSA        G02INV
061300951003     C                   MOVE      '3'           G02ERR
061400951003     C                   CALL      'XSRDA8'
061500951003     C                   PARM                    WLBDA8
061600951003     C                   Z-ADD     G02DAT        DATAAL
061700951003     C                   TIME                    TIMES             6 0
061800021011      *
061900150710     C                   Z-ADD     d52DSD        KMGS
062000150710     C                   MOVEL     d52DSD        KAAS
062100021011      *
062200150710     C                   Z-ADD     d52DSD        da_mgs
062300150710     C                   MOVEL     d52DSD        da_aas
062400021011      *
062500041006     C                   MOVEL     *ALL'-'       CMP198
062600021011      *
062700951010      *  Azzero DS multipla totali
062800951010     C                   Z-ADD     1             X                 4 0
062900090409     C     X             DOWLE     200
063000951010     C     EVE(X)        ANDNE     *BLANKS
063100951010     C     X             OCCUR     TOT
063200951010     C                   Z-ADD     0             WTGIAC
063300951010     C                   Z-ADD     0             WTOGAP
063400041006     C                   Z-ADD     0             Wtgcsp
063500041006     C                   Z-ADD     0             Wtgnsp
063600951010     C                   ADD       1             X
063700951010     C                   END
063800951010     C                   Z-ADD     0             WTTSPE
063900951010     C                   Z-ADD     0             WTTGIA
064000951010     C                   Z-ADD     0             WTTRES
064100951010     C                   Z-ADD     0             WTTGAP
064200041006     C                   Z-ADD     0             WTTcsp
064300041006     C                   Z-ADD     0             WTTnsp
064400021011      *  Eseguo stampa prima pagina
064500021011     C                   EXSR      STPINI
064600951010      *
064700951003     C                   ENDSR
064800021011?     *--------------------------------------------------------------*
064900021011?     *  STPINI: Stampa prima pagina                                 *
065000021011?     *--------------------------------------------------------------*
065100021011     C     STPINI        BEGSR
065200021011      *
065300021011     c                   clear                   y                 3 0
065400021011      *
065500021011     C                   WRITE     PRIPAG
065600021011     C                   WRITE     DETSPA
065700021011      *
065800021011     C                   DO        20            X
065900021011      *
066000021011     C     KSC(X)        IFNE      0
066100021011     C                   Z-ADD     KSC(X)        STPKSC
066200021011     c                   add       1             y
066300021011      *  x cliente
066400150710     C     d52MOD        IFEQ      'C'
066500021011     C                   Z-ADD     CLIITA        KKCC
066600021011     C                   Z-ADD     KSC(X)        KKSC
066700021011     C                   MOVEL     *BLANKS       ACORAG
066800021011     C     KACO          CHAIN     CNACO00F                           31
066900021011     C                   MOVEL     ACORAG        STPDES
067000021011     C                   ELSE
067100021011      *  x LNP
067200021011     C                   Z-ADD     KSC(X)        ORGFIL
067300021011     C                   MOVEL     *BLANKS       ORGDES
067400021011     C     ORGFIL        CHAIN     AZORG01L                           31
067500021011     C                   MOVEL     ORGDES        STPDES
067600021011     C                   END
067700021011      *
067800021011     C                   WRITE     DETCLI
067900021011     C                   WRITE     DETSPA
068000021011      *
068100021011     C                   END
068200021011     C                   END
068300021011     C*
068400021011     C                   WRITE     FINPAG
068500021011     C                   CLOSE     CNACO00F
068600021011     C*
068700021011     C                   ENDSR
068800951003      *--------------------------------------------------------------*
068900951003     O*  STAMPA ERRORI !!!!!!!!
069000951003      *--------------------------------------------------------------*
069100951003     OSYSPRT    E            TESERR           01
069200951003     O                       STT(1)              66
069300951003     O                       STT(2)             132
069400951003     O                       RSUT                20
069500951003     O                       UDATE              118 '  /  /  '
069600951003     O                       PAGE          Z    132
069700951003     O          E            TESERR           03
069800951003     O                       TIMES              118 '  .  .  '
069900951003     O          E            TESERR           06
070000951003     O                                           24 'SI RACCOMANDA DI CONSEGN'
070100951003     O                                           48 'ARE QUESTA STAMPA ALL''U'
070200951003     O                                           72 'FFICIO  C.E.D.!!!!!!    '
070300951003     O          E            TESERR      0  2
070400951003     O                                           24 'SI RACCOMANDA DI CONSEGN'
070500951003     O                                           48 'ARE QUESTA STAMPA ALL''U'
070600951003     O                                           72 'FFICIO  C.E.D.!!!!!!    '
070700951003     O          E            DETERR         1
070800951003     O                                           24 'NON CARICATI TUTTI GLI E'
070900951003     O                                           48 'LEMENTI DELLA TABELLA 2A'
071000951003      *--------------------------------------------------------------*
071100990628** STT
071200951003                                 ***  S T A M P A     G I A C E N
071300070725Z E      P E R     C L I E N T E  ***  FNLG53R              PAG.
