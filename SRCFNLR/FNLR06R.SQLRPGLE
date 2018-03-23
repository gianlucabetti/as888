000100070112      *PARMS CLOSQLCSR(*ENDMOD)
000200070112      *PARMS DYNUSRPRF(*OWNER)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400941012     H* FNLR06R *----------------------------------------------------*
000500941026     H*        - STAMPA SPUNTA                    ESEGUE             *
000600941027     H*          STAMPA SEGNACOLLI CON ANOMALIA
000700000000     H*--------------------------------------------------------------*
000800941020     H* VIENE LANCIATO ANCHE DA I.M.A. (FNLR30R FILTRO)              *
000900941020     H*--------------------------------------------------------------*
001000070115     FFNBRV02L  IF   E           K DISK
001100070112     F**FIBRV05L  IF   E           K DISK
001200070112     F**                                     RENAME(FIBRV000:FIBRV005)
001300941012     FFNFVV01L  IF   E           K DISK
001400941027     FFNFVA01L  IF   E           K DISK
001500020927     fFNFWA01L  if   e           k disk    rename(FNFWA000:FNFWA001)
001600020927     fFNFWA02L  if   e           k disk    rename(FNFWA000:FNFWA002)
001700941027     FFNFGV01L  IF   E           K DISK
001800020927     fFNFGW01L  if   e           k disk    rename(FNFGW000:FNFGW001)
001900020927     fFNFGW02L  if   e           k disk    rename(FNFGW000:FNFGW002)
002000900626     FAZORG01L  IF   E           K DISK
002100900626     FTABEL00F  IF   E           K DISK
002200941020     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002300941020     F                                     USROPN
002400020808
002500920206     D SPU             S              7  0 DIM(9)                               SPUNTA
002600920206     D CAN             S              2    DIM(9)                               COD ANOMALIA
002700910321     D LA1             S             16    DIM(8)                               LEGENDA ANOM
002800910321     D LA2             S             16    DIM(8)                               LEGENDA ANOM
002900920805     D NPS             S              2    DIM(60)                              LETT BARCOD
003000920805     D CPS             S              7  0 DIM(60)                              COLLI LETTI
003100941027     D NPM             S              2  0 DIM(100)                             PISTOLE MANUAL
003200941027     D*
003300941028     D SK              S             66    DIM(15) CTDATA PERRCD(1)             SKIERE STAMPA
003400941020     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
003500941027     D CM2             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
003600941027     D*
003700941027     D L1              S              3  0 DIM(30)                              FIL GESTITE
003800020808
003900911030     D WLBDAT          DS
004000941013     D  G02DAT                 1      8  0
004100941013     D  G02INV                 9     16  0
004200941013     D  G02ERR                17     17
004300941013     D  G02TGI                18     22  0
004400900626     D                 DS
004500070115     D  BRVZNC_D               1      2  0
004600070115     D  BRVLNP_D               3      5  0
004700070115     D  BRVNRS_D               6      7  0
004800941028     D  CONLNP                 3      7  0
004900941027     D  CONBRV                 1      7  0
005000941027     D                 DS
005100941027     D  SAVZNC                 1      2  0
005200941027     D  SAVLNP                 3      5  0
005300941027     D  SAVNRS                 6      7  0
005400941028     D  SAVLIN                 3      7  0
005500941027     D  SAVBRV                 1      7  0
005600910320     D                 DS
005700910320     D  FL3E                   1      1
005800910320     D  UGUA                   2      2
005900910320     D  DE3E                   3     14
006000910320     D  COMCAN                 1     14
006100020808
006200941027     D* DS PER TRUL06R - CARICAMENTO £X
006300941027     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006400941027     D  LIN                    1     90  0
006500941027     D                                     DIM(30)                              SKI COMODO
006600941027     D* DS STANDARD FOGLI VARI
006700941027     D DSLR01        E DS
006800941027     D*
006900161006     D DS7B          E DS
007000161006     D DS7J          E DS
007100090331     D DS3E          E DS
007200941027     D KPJBA         E DS
007300900621     D CNCR80        E DS
007400900621     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007500020927     d OG143         e ds
007600050512     d dfvvspn       e ds
007700020927     d* Parametri per pgm. FNLV55R
007800020927     d FNLV55DS      e ds                  inz
007900020927     d  D55tpt       e                     inz('P')
008000070115     d FnbrvxxDs     e ds                  ExtName(Fnbrv00f)
008100020808
008200020808     D**
008300020808     D* DEFINIZIONE VARIABILI
008400020808     D**
008500020808     D COD             S                   LIKE(TBLCOD)
008600020808     D KEY             S                   LIKE(TBLKEY)
008700020808     D STADFV          S                   LIKE(FVVDFV)                         DATA FOGLIO VIAGGIO
008800020808     D WDFV            S                   LIKE(FVVDFV)                         DATA FOGLIO VIAGGIO
008900020808     D WNFV            S                   LIKE(BRVNFV)                         NUM. FOGLIO VIAGGIO
009000020808     d COMfil          s                   like(FVVfgs) inz
009100070112     d lnapre          s                   like(brvlna) inz
009200020808     D XX              S              3  0 inz
009300020808     D XY              S              2  0 inz
009400020808     D XI              S              3  0 inz
009500020808
009600941028     D**
009700941028     D* DEFINIZIONE COSTANTI
009800941028     D**
009900941028     D CTRAZ           C                   CONST('Trazionista:')
010000941028     D CSPUNT          C                   CONST('Spuntatore :')
010100020808
010200000000     C*---------------------------------------------------------------*
010300900622     C     *ENTRY        PLIST
010400900622     C                   PARM                    KPJBA
010500941027     C                   MOVEL     KPJBU         DSLR01
010600000125     C                   Z-ADD     DLRNFV        WNFV
010700941020     C*
010800000000     C                   Z-ADD     1             CODUT
010900900621     C                   CALL      'X§PARUT'
011000900621     C                   PARM                    UT§DSE
011100941020     C                   MOVEL     RAGUT         RSUT             20
011200900621     C                   MOVEL     REC80         CNCR80
011300900828     C*-----------------------------------------------------
011400941027     C* OPERAZIONI INIZIALI
011500910321     C                   EXSR      INIZIO
011600941027     C*
011700941027     C     *INH1         IFEQ      *OFF
011800020927     c     Wok           andeq     'S'
011900900626     C*
012000941020     C* UNA SOLA LINEA DI ARRIVO ?
012100941027     C     DLRLNA        COMP      0                                  06        ON=UNA SOLA
012200941020     C*
012300070112     c                   if        *in01 = '0'
012400070112     c                   exsr      sr_noanom
012500070112     c                   else
012600070112     c                   exsr      sr_sianom
012700070112     c                   endif
012800070112     c*
012900941027     C                   ENDIF
013000910320     C*
013100000000     C                   SETON                                        LR
013200070112     C**************************************************************************
013300070112     c     sr_noanom     begsr
013400070115     C     KBRV2         SETLL     FNBRV02L
013500070115     C  N06KBRV1         READE     FNBRV02L                               30
013600070115     C   06KBRV2         READE     FNBRV02L                               30
013700070112    1C     *IN30         DOWEQ     *OFF
013800070112     C*
013900070112     C* PULIZIA CAMPI DI TOTALE
014000070112     C                   Z-ADD     0             TOTSPU
014100070112     C                   Z-ADD     1             XY
014200070112     C                   MOVEA     *ZEROS        CPS
014300070112     C                   MOVEL     *BLANKS       NPS
014400070112     C                   Z-ADD     0             XX
014500070112     C*
014600070112    2C     *IN30         DOWEQ     *OFF
014700070112     C*
014800070112     c                   exsr      sr_elabora
014900070112     C*
015000070115     C     KBRV          READE     FNBRV02L                               30
015100070112    2C                   ENDDO
015200070112     C*
015300070112     C* SE CI SONO SPUNTE STAMPO
015400070112    2C     SPU(1)        IFNE      0
015500070112     C                   EXSR      STAMPA
015600070112    2C                   END
015700070112     C* STAMPO TOTALE SPUNTE SE CE NE SONO
015800070112    2C     TOTSPU        IFNE      0
015900070112     C                   EXSR      TOTALI
016000070112    2C                   END
016100070112     C*
016200070112     C* CONTROLLO SE C'E' NUOVA LINEA DI ARRIVO
016300070112     C                   SETOFF                                       020304
016400070112     C*
016500070112     C                   if        *in06 = '0'
016600070115     C     KBRV          SETGT     FNBRV02L
016700070115     C     KBRV1         READE     FNBRV02L                               30
016800070112     c                   endif
016900070112    1C                   ENDDO
017000070112     c                   endsr
017100070112     C**************************************************************************
017200070112     c     sr_sianom     begsr
017300070112     c                   clear                   lnapre
017400070112    1c                   if        dlrlna = 0
017500070112     C/EXEC SQL
017600070112     C+ declare C1 cursor for
017700070115     c+ select * from fnbrv02l where brvnpg = :dlrnpg and
017800070112     c+ brvnfv= :Wnfv and brvfgs= :comfil
017900070112     c+ order by brvnpg, brvnfv, brvfgs, brvlna, brvznc, brvlnp,
018000070112     c+ brvnrs, brvnsc for fetch only
018100070112     C/END-EXEC
018200070112     c* apertura cursore
018300070112     c*
018400070112     C/EXEC SQL
018500070112     C+ Open C1
018600070112     C/END-EXEC
018700070112   x1c                   else
018800070112     C/EXEC SQL
018900070112     C+ declare C2 cursor for
019000070115     c+ select * from fnbrv02l where brvnpg = :dlrnpg and
019100070112     c+ brvnfv= :Wnfv and brvfgs= :comfil and brvlna= :dlrlna
019200070112     c+ order by brvnpg, brvnfv, brvfgs, brvlna, brvznc, brvlnp,
019300070112     c+ brvnrs, brvnsc for fetch only
019400070112     C/END-EXEC
019500070115     C* PULIZIA CAMPI DI TOTALE
019600070115     C                   Z-ADD     0             TOTSPU
019700070115     C                   Z-ADD     1             XY
019800070115     C                   MOVEA     *ZEROS        CPS
019900070115     C                   MOVEL     *BLANKS       NPS
020000070115     C                   Z-ADD     0             XX
020100070112     c* apertura cursore
020200070112     c*
020300070112     C/EXEC SQL
020400070112     C+ Open C2
020500070112     C/END-EXEC
020600070112    1c                   endif
020700070112     c*
020800070112     c*
020900070112     c* lettura cursore
021000070112     c*
021100070112    1c                   do        *hival
021200070112     c                   if        dlrlna = 0
021300070112     C/EXEC SQL
021400070115     C+ Fetch C1 into :fnbrvxxds
021500070112     C/END-EXEC
021600070112     c                   else
021700070112     C/EXEC SQL
021800070115     C+ Fetch C2 into :fnbrvxxds
021900070112     C/END-EXEC
022000070112     c                   endif
022100070112    2c                   select
022200070112     c                   when      sqlcod = 100  or sqlcod < 0
022300070112     c                   leave
022400070115     c                   when      sqlcod = 0
022500070112     c* OPERAZIONI A ROTTURA DI LNA
022600070112     c* se non è stata richiesta una specifica lna a cambio lna devo
022700070112     c* eseguire le operazioni a totale
022800070112    3c                   if        not *in06  and lnapre <> brvlna
022900070112    4c                   if        lnapre > 0
023000070112     C* SE CI SONO SPUNTE STAMPO
023100070112    5C     SPU(1)        IFNE      0
023200070112     C                   EXSR      STAMPA
023300070112    5C                   END
023400070112     C* STAMPO TOTALE SPUNTE SE CE NE SONO
023500070112    5C     TOTSPU        IFNE      0
023600070112     C                   EXSR      TOTALI
023700070112    5C                   END
023800070112    4c                   endif
023900070112     C                   SETOFF                                       020304
024000070112     C* PULIZIA CAMPI DI TOTALE
024100070112     C                   Z-ADD     0             TOTSPU
024200070112     C                   Z-ADD     1             XY
024300070112     C                   MOVEA     *ZEROS        CPS
024400070112     C                   MOVEL     *BLANKS       NPS
024500070112     C                   Z-ADD     0             XX
024600070112     c                   move      brvlna        lnapre
024700070112    3c                   endif
024800070112     c                   exsr      sr_elabora
024900070112    2c                   endsl
025000070112
025100070112    1c                   EndDo
025200070112     C* SE CI SONO SPUNTE STAMPO
025300070112    2C     SPU(1)        IFNE      0
025400070112     C                   EXSR      STAMPA
025500070112    2C                   END
025600070112     C* STAMPO TOTALE SPUNTE SE CE NE SONO
025700070112    2C     TOTSPU        IFNE      0
025800070112     C                   EXSR      TOTALI
025900070112    2C                   END
026000070112     c                   if        dlrlna = 0
026100070112     C/EXEC SQL
026200070112     C+ close C1
026300070112     C/END-EXEC
026400070112     c                   else
026500070112     C/EXEC SQL
026600070112     C+ close C2
026700070112     C/END-EXEC
026800070112     c                   endif
026900070112     c                   endsr
027000070112     C**************************************************************************
027100070112     c     sr_elabora    begsr
027200070112     C* VERIFICO LE ESCLUSIONI
027300070112     C                   EXSR      ESCLUS                                       ESCLUSIONI
027400070112     C*
027500070112    3C     WOK           IFEQ      'S'
027600070112     C*
027700070112     C* IMPOSTO LA LINEA PARTENZA ARRIVO
027800070112     C  N02              EXSR      IMPO
027900070112     C  N02              SETON                                        02
028000070112     C*
028100070115     c                   z-add     brvlnp        brvlnp_d
028200070115     c                   z-add     brvznc        brvznc_d
028300070115     c                   z-add     brvnrs        brvnrs_d
028400070112     C* SE CAMBIA SERIE O LINEA PARTENZA STAMPO RIGA DELLA LINEA
028500070112    4C  N01CONLNP        IFNE      SAVLIN
028600070112     C                   EXSR      CAMLNP
028700070112    4C                   END
028800070112     C*
028900070112    4C   01CONBRV        IFNE      SAVBRV
029000070112     C                   EXSR      CAMLNP
029100070112    4C                   END
029200070112     C**
029300070112     C* CARICO SCHIERE SPUNTA
029400070112     C                   ADD       1             XX
029500070112     C                   ADD       1             TOTSPU            9 0
029600070112     C                   Z-ADD     BRVNSC        SPU(XX)
029700070112     C                   MOVE      BRVCAN        CAN(XX)
029800070112     C*
029900070112     C* SE PISTOLA MANUALE LA INDICO CON L'*
030000070112     C                   MOVEL     ' '           CAN(XX)
030100070112     C     BRVNPS        LOOKUP    NPM                                    31
030200070112    4C     *IN31         IFEQ      *ON
030300070112     C                   MOVEL     '*'           CAN(XX)
030400070112    4C                   END
030500070112     C*
030600070112     C* MEMORIZZO TOTALI PER LETTORE BARCODE
030700070112     C                   Z-ADD     1             XI
030800070112     C                   MOVEL     BRVNPS        COMNPS            2
030900070112     C     COMNPS        LOOKUP    NPS(XI)                                31
031000070112     C*
031100070112     C* 31 OFF - NON ESISTE AGGIUNGO
031200070112     C  N31              MOVEL     COMNPS        NPS(XY)
031300070112     C  N31              Z-ADD     XY            XI
031400070112     C  N31              ADD       1             XY
031500070112     C*
031600070112     C                   ADD       1             CPS(XI)
031700070112     C* SE CARICATE 9 SPUNTE STAMPO RIGA DETTAGLIO
031800070112    4C     XX            IFEQ      9
031900070112     C                   EXSR      STAMPA
032000070112    4C                   END
032100070112     C*
032200070112    3C                   ENDIF                                                  WOK = 'S'
032300070112     c                   endsr
032400941020     C**************************************************************************
032500941020     C*   ESCLUSIONI DURANTE LE LETTURE
032600941020     C**************************************************************************
032700941020     C     ESCLUS        BEGSR
032800941020     C*
032900941020     C                   MOVEL     'S'           WOK               1            DI COMODO
033000941020     C*
033100941020     C* NR.PISTOLA SPUNTA-COLLI
033200941027     C     BRVNPS        IFLT      DLRNPD                                       PISTOLA INIZIALE
033300941027     C     BRVNPS        ORGT      DLRNPA                                       PISTOLA FINALE
033400941020     C                   MOVEL     *BLANKS       WOK                            DA ESCLUDERE
033500941020     C                   ENDIF
033600941020     C*
033700941020     C* SERIE
033800941027     C     BRVNRS        IFLT      DLRSRD                                       SERIE INIZIALE
033900941027     C     BRVNRS        ORGT      DLRSRA                                       SERIE FINALE
034000941020     C                   MOVEL     *BLANKS       WOK                            DA ESCLUDERE
034100941020     C                   ENDIF
034200941027     C*
034300941027     C* SE SEGNACOLLI CON ANOMALIA ESCLUDO SE NON HA ANOMALIA
034400941027     C   01BRVCAN        IFEQ      ' '
034500941027     C                   MOVEL     *BLANKS       WOK                            DA ESCLUDERE
034600941027     C                   ENDIF
034700941020     C*
034800941020     C                   ENDSR
034900941012     C**************************************************************************
035000941027     C*   O P E R A Z I O N I    I N I Z I A L I
035100941027     C*
035200941027     C*   RICHIAMO DEL FOGLIO VIAGGIO DA STAMPARE:
035300941027     C*  ?  .. SU FNFGV  <---  SE CATEGORIA = 1 & LNP=SIMFEL     ?
035400941027     C*  ?  .. SU FNFVA  <---  SE CATEGORIA = 1 & LNP NON IN £1  ?
035500941027     C*  ?  .. SU FNFVV  <---  SE CATEGORIA > 1                  ?
035600941012     C**************************************************************************
035700910321     C     INIZIO        BEGSR
035800020927     c*
035900020927     c                   move      *date         G02DAT
036000020927     c                   clear                   G02ERR
036100020927     c                   call      'XSRDA8'
036200020927     c                   parm                    WLBDAT
036300020927     c                   z-add     G02inv        WDATEU            8 0          AAAA/MM/GG
036400941027     C***
036500941027     C* CARICO TABELLA FILIALI GESTITE £1
036600941027     C***
036700941027     C                   CLEAR                   DSUL06
036800941027     C                   MOVE      '£1'          D06COD
036900941027     C                   MOVEL     SIMFEL        D06KEY
037000941027     C                   MOVEL     'L'           D06TLA
037100941027     C                   MOVEL     DSUL06        KPJBU
037200941027     C*
037300941027     C                   CALL      'TRUL06R'
037400941027     C                   PARM                    KPJBA
037500941027     C                   MOVEL     KPJBU         DSUL06
037600941027     C                   MOVEA     LIN           L1
037700020808     C**
037800020808     C* DECODIFICA FIL GESTIONE
037900020808     C**
038000020808     C     DLRFGS        CHAIN     AZORG01L                           31
038100020808     C                   MOVEL     ORGDES        DESFEL           20
038200020927     c                   movel     ORGde3        OG143
038300020927     c                   reset                   FNLV55ds
038400020927     c                   if        §OGntw = 'PPT'
038500020927     c                   eval      D55lin = DLRfgs
038600020927     c                   eval      D55drf = WDATEU
038700020927     c                   call      'FNLV55R'
038800020927     c                   parm                    FNLV55DS
038900020927     c***                if        D55err <> *blanks
039000020927     c***                eval      D55tfp =  D55lin
039100020927     c***                endif
039200020927     c                   endif
039300020927     c                   if        §OGntw = 'PPT'
039400020927     c                             and DLRnpg = 1
039500020927     c                   z-add     D55tfp        COMfil
039600020927     c                   else
039700020927     c                   z-add     DLRfgs        COMfil
039800020927     c                   endif
039900020927     c*
040000020927     c                   eval      wOK    = 'S'
040100941027     C**
040200941027     C* CHAIN FOGLIO
040300941027     C**
040400941027     C                   CLEAR                   WDFV
040500941027     C*
040600941027    1C     DLRNPG        IFEQ      1
040700941027     C*
040800941027     C     DLRFGS        LOOKUP    L1                                     30
040900941027     C*
041000941027    2C     *IN30         IFEQ      *ON
041100020808     ***C     DLRNFV        CHAIN     FNFGV000                           31
041200020927     c                   eval      FGVlnp = DLRfgs
041300020927      * se p.o. poste reperisco il p.o. di partenza
041400020927    3c                   if        §OGntw = 'PPT'
041500020927     c     KFVA          chain     FNFGW02L                           31
041600020927     c  N31FGWatb        comp      *blanks                            3131
041700020927     c  N31              eval      FGVlnp = FGWlnp
041800020927     c   31              eval      Wok    = *blanks
041900020927    3c                   endif
042000020808     C     K02FGV01      CHAIN     FNFGV000                           31
042100020927      * se p.o. non poste controllo che il p.o. gestione non sia poste
042200020927    3c                   if        not *in31
042300020927     c                             and §OGntw <> 'PPT'
042400020927     c     K02FGV01      chain     FNFGW01L
042500020927    4c                   if        %found(FNFGW01L)
042600020927     c                             and FGWatb =  *blanks
042700020927     c                             and FGWfgs <> *zeros
042800020927     c                             and FGWfgs <> FGVlnp
042900020927     c                   eval      *in31 = *on
043000020927    4c                   endif
043100020927    3c                   endif
043200020927     c   31              eval      Wok    = *blanks
043300941028     C*
043400941028     C     *IN31         IFEQ      *OFF
043500941028     C                   MOVEL     FGVDFV        WDFV
043600941028     C* IMPOSTO NELLA STAMPA IL TRASIONISTA
043700941028     C     FGVDPD        IFNE      *BLANKS
043800941028     C                   MOVEL     CTRAZ         DECSPU           12
043900941028     C                   MOVEL     FGVDPD        DESSPU           20
044000941028     C                   SETON                                        1617
044100941028     C                   ENDIF
044200941028     C                   ENDIF
044300941028     C*
044400941027   X2C                   ELSE
044500941027     C*
044600941027     C* FOGLIO NON DELL'AREA : CHAIN SU FNFVA
044700020808     ***C     KFVA          CHAIN     FNFVA000                           31
044800020808     ***C  N31              MOVEL     FVADFV        WDFV
044900020927     c                   eval      FVAlnp = DLRfgs
045000020927      * se p.o. poste reperisco il p.o. di partenza
045100020927    3c                   if        §OGntw = 'PPT'
045200020927     c     KFVA          chain     FNFWA02L                           31
045300020927     c  N31FWAatb        comp      *blanks                            3131
045400020927     c  N31              eval      FVAlnp = FWAlnp
045500020927     c   31              eval      Wok    = *blanks
045600020927    3c                   endif
045700020808     c     K02FVA01      CHAIN     FNFVA000                           31
045800020927      * se p.o. non poste controllo che il p.o. gestione non sia poste
045900020927    3c                   if        not *in31
046000020927     c                             and §OGntw <> 'PPT'
046100020927     c     K02FVA01      chain     FNFWA01L
046200020927    4c                   if        %found(FNFWA01L)
046300020927     c                             and FWAatb =  *blanks
046400020927     c                             and FWAfgs <> *zeros
046500020927     c                             and FWAfgs <> FVAlnp
046600020927     c                   eval      *in31 = *on
046700020927    4c                   endif
046800020927    3c                   endif
046900020927     c   31              eval      Wok    = *blanks
047000020927     c*
047100020808    3c     *IN31         IFEQ      *OFF
047200020808     c                   MOVEL     FVADFV        WDFV
047300020927     c* IMPOSTO NELLA STAMPA IL TRAZIONISTA
047400020927     c     FVADPD        IFNE      *BLANKS
047500020927     c                   MOVEL     CTRAZ         DECSPU
047600020927     c                   MOVEL     FVADPD        DESSPU
047700020927     c                   SETON                                        1617
047800020927     c                   ENDIF
047900020808    3c                   ENDIF
048000941027    2C                   ENDIF
048100941027     C*
048200941027   X1C                   ELSE
048300941027     C* NON E' FOGLIO PARTENZA
048400941027     C     KFVV          CHAIN     FNFVV01L                           31
048500941028     C*
048600941028     C     *IN31         IFEQ      *OFF
048700941028     C                   MOVEL     FVVDFV        WDFV
048800050512     C                   MOVEL     FVVSPN        dfvvspn
048900050512     C     §fvvspn       IFNE      *BLANKS
049000941028     C                   MOVEL     CSPUNT        DECSPU
049100050512     C                   MOVEL(P)  §fvvspn       DESSPU
049200941028     C                   SETON                                        1617
049300941028     C                   ENDIF
049400941028     C*
049500941028     C* SE CATEORIGA INVENTARIO DECOCIFICO ANCHE LA SOTTO CATEGORIA
049600941028     C     FVVSPG        IFNE      ' '
049700941028     C                   SETON                                        1517
049800941028     C*
049900941028     C                   MOVEL     '7B'          COD
050000941028     C                   MOVEL(P)  FVVSPG        KEY
050100941028     C     KTAB2         CHAIN     TABEL                              30
050200161006     c   30              clear                   ds7b
050300161006     C  N30              MOVEL     TBLUNI        ds7b
050400161006     C                   MOVEL     §7bdes        DESINV           15
050500941028     C                   END
050600941027    1C                   ENDIF
050700941028     C                   ENDIF
050800020927     ***C**
050900020927     ***C* DECODIFICA FIL GESTIONE
051000020927     ***C**
051100020927     ***C     DLRFGS        CHAIN     AZORG01L                           31
051200020927     ***C                   MOVEL     ORGDES        DESFEL           20
051300020927     *** * >>>>> spostata prima <<<<<
051400941027     C**
051500941027     C* DECODIFICA CATEGORIA FOGLIO
051600941027     C**
051700920807     C                   MOVEL     '7N'          COD
051800941027     C                   MOVEL(P)  DLRNPG        KEY
051900920807     C     KTAB2         CHAIN     TABEL                              31
052000920807     C  N31              MOVEL     TBLUNI        STACFV
052100941028     C   31              MOVEL     *BLANKS       STACFV           12
052200920807     C*
052300941027     C**
052400911030     C* DATA FOGLIO VIAGGIO
052500941027     C**
052600941027     C                   Z-ADD     WDFV          G02INV                         AAAA/MM/GG
052700941020     C                   Z-ADD     *ZEROS        G02DAT
052800941020     C                   MOVEL     '3'           G02ERR
052900941020     C                   CALL      'XSRDA8'
053000911030     C                   PARM                    WLBDAT
053100941020     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
053200910321     C*
053300910321     C* PRENDO FLAG ANOMALIA PRIMI 8
053400090331     c* prendo solo i codici ancora utilizzabili
053500910321     C                   MOVEL     '3E'          COD
053600910321     C                   MOVEL     '='           UGUA
053700020808     C                   Z-ADD     1             XI
053800910321     C     KTAB          SETLL     TABEL                                  31
053900020808     C   31XI            DOWLE     8
054000910321     C     KTAB          READE     TABEL                                  31
054100910321     C  N31              DO
054200910321     C                   MOVEL     TBLKEY        FL3E
054300910321     C                   MOVEL     TBLUNI        DE3E
054400090331     C                   MOVEL     TBLUNI        Ds3e
054500090331     c                   if        §3euti<>'N'
054600090331     c* e e' cat 8 carico tutti altrimenti solo quelli non per 8
054700090331     c                   if        (dlrnpg<>8 and §3ecat8=' ') or
054800090331     c                              dlrnpg=8
054900020808     C                   MOVEL     COMCAN        LA1(XI)
055000020808     C                   ADD       1             XI
055100090331     c                   endif
055200090331     c                   endif
055300090331     c
055400090331     C                   ENDDO
055500910321     C* FINE LETTURA
055600020808     C   31              Z-ADD     20            XI
055700910321     C                   END
055800020808     C  N31              Z-ADD     1             XI
055900910321     C*
056000910321     C* PRENDO FLAG ANOMALIA DA 9 A 16
056100020808     C     XI            DOWLE     8
056200910321     C     KTAB          READE     TABEL                                  31
056300910321     C  N31              DO
056400910321     C                   MOVEL     TBLKEY        FL3E
056500910321     C                   MOVEL     TBLUNI        DE3E
056600090331     C                   MOVEL     TBLUNI        Ds3e
056700090331     c                   if        §3euti<>'N'
056800090331     c* e e' cat 8 carico tutti altrimenti solo quelli non per 8
056900090331     c                   if        (dlrnpg<>8 and §3ecat8=' ') or
057000090331     c                              dlrnpg=8
057100020808     C                   MOVEL     COMCAN        LA2(XI)
057200020808     C                   ADD       1             XI
057300090331     c                   endif
057400090331     c                   endif
057500910321     C                   END
057600910321     C* FINE LETTURA
057700020808     C   31              Z-ADD     20            XI
057800910321     C                   END
057900910321     C*
058000941020     C* RILEVO LA DATA E L'ORA PER LA STAMPA
058100941020     C                   TIME                    WTIME            14 0          ORA E DATA
058200941020     C                   MOVE      WTIME         WUDATE            8 0          GG/MM/AAAA
058300941020     C                   MOVEL     WTIME         UTIME             6 0          ORARIO
058400910321     C                   Z-ADD     0             BRVLNA
058500941027     C***
058600941027     C* CARICO LE PISTOLE MANUALI
058700941027     C***
058800941027     C                   MOVEL     '7J'          COD
058900020808     C                   Z-ADD     1             XI
059000941027     C     KTAB          SETLL     TABEL
059100941027     C     KTAB          READE     TABEL                                  30
059200941027     C*
059300941027     C     *IN30         DOWEQ     *OFF
059400941027     C     TBLFLG        IFEQ      ' '
059500941027     C                   MOVEL     TBLUNI        DS7J
059600941027     C* SE MANUALE LA MEMORIZZO
059700941027     C     §7JRPS        IFEQ      'M'
059800020808     C                   MOVEL     TBLKEY        NPM(XI)
059900020808     C                   ADD       1             XI
060000941027     C                   ENDIF
060100941027     C                   ENDIF
060200941027     C*
060300941027     C     KTAB          READE     TABEL                                  30
060400941027     C                   ENDDO
060500941027     C**
060600941027     C* DLRFS1 = 'S' STAMPA SPUNTA = 'A' STAMPA SPUNTE CON ANOMALIA
060700941027     C**
060800941027     C     DLRFS1        IFEQ      'A'
060900941027     C                   SETON                                        01
061000941027     C                   ENDIF
061100941027     C**
061200941027     C* PREPARO STAMPANTE
061300941027     C**
061400941027     C                   Z-ADD     80            LUNG             15 5
061500941027     C                   MOVEL     *BLANKS       COMMAN           80
061600941027     C  N01              MOVEA     CMD           COMMAN
061700941027     C   01              MOVEA     CM2           COMMAN
061800941027     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
061900941027     C                   PARM                    COMMAN
062000941027     C                   PARM                    LUNG
062100941027     C*
062200941027     C  NH1              OPEN      QSYSPRT
062300910321     C*
062400910321     C                   ENDSR
062500941012     C**************************************************************************
062600941012     C*    CAMBIA LINEA PARTENZA
062700941012     C**************************************************************************
062800900626     C     CAMLNP        BEGSR
062900941012     C*
063000900626     C* SE CI SONO SPUNTE STAMPO
063100900626     C     SPU(1)        IFNE      0
063200900626     C                   EXSR      STAMPA
063300900626     C                   END
063400941031     C*
063500941031     C* PER SEGNACOLLI CON ANOMALIA, SE NON ACCESO OF STAMPO LA NUOVA
063600941031     C* ZONA
063700941031     C     *IN01         IFEQ      *ON
063800941031     C     *INOF         ANDEQ     *OFF
063900941031     C     BRVZNC        ANDNE     SAVZNC
064000941031     C                   MOVE      BRVZNC        SAVZNC
064100941031     C                   EXCEPT    TES3
064200941031     C                   ENDIF
064300941031     C* SALVO NUOVA LINEA
064400941031     C                   MOVEL     BRVLNP        SAVLNP
064500941031     C                   MOVE      BRVNRS        SAVNRS
064600941031     C                   MOVE      BRVZNC        SAVZNC
064700900626     C*
064800900626     C* DECODIFICO LINEA PARTENZA
064900941012     C     BRVLNP        CHAIN     AZORG01L                           31
065000920206     C  N31              MOVEL     ORGDES        DESLNP           20
065100900626     C   31              MOVEL     *BLANKS       DESLNP
065200900626     C* STAMPO TESTATA
065300900626     C                   SETOFF                                       03
065400900626     C                   ENDSR
065500941012     C**************************************************************************
065600941012     C* IMPOSTAZIONE INIZIALE
065700941012     C**************************************************************************
065800900621     C     IMPO          BEGSR
065900941012     C*
066000920521     C* SALVO PRIMA LINEA
066100920521     C                   Z-ADD     BRVLNA        SAVLNA            3 0
066200941012     C     SAVLNA        CHAIN     AZORG01L                           32
066300920521     C  N32              MOVEL     ORGDES        DESLNA           20
066400920521     C   32              MOVEL     *BLANKS       DESLNA
066500941027     C*
066600900626     C* SALVO PRIMA LINEA PARTENZA E SERIE
066700941027     C                   MOVEL     BRVLNP        SAVLNP
066800941027     C                   MOVE      BRVNRS        SAVNRS
066900941027     C                   MOVE      BRVZNC        SAVZNC
067000941027     C*
067100900621     C* DECODIFICO LINEA PARTENZA
067200941012     C     BRVLNP        CHAIN     AZORG01L                           31
067300920206     C  N31              MOVEL     ORGDES        DESLNP           20
067400900622     C   31              MOVEL     *BLANKS       DESLNP
067500900626     C*
067600941027     C* TESTATA SOLO PER STAMPA SPUNTE
067700941027     C     *IN01         IFEQ      *OFF
067800901114     C                   EXCEPT    TES2
067900941028     C                   EXCEPT    TES4
068000941028     C                   SETON                                        04
068100941027     C                   ELSE
068200941027     C                   SETON                                        OF
068300941027     C                   ENDIF
068400900626     C*
068500900621     C                   ENDSR
068600941012     C**************************************************************************
068700941012     C* STAMPO SPUNTA
068800941012     C**************************************************************************
068900900621     C     STAMPA        BEGSR
069000941012     C*
069100941027     C* STAMPO TESTATA
069200900622     C   OF              DO
069300941027     C**
069400941027     C* LA PRIMA VOLTA NON STAMPO I PIEDI
069500941028     C     *IN04         IFEQ      *ON
069600901114     C                   EXCEPT    PIEDI
069700941027     C                   ENDIF
069800941027     C**
069900941027     C                   SETOFF                                       03
070000941028     C                   SETON                                        0418
070100901114     C                   EXCEPT    TES2
070200941028     C   01              EXCEPT    TES3
070300941028     C                   EXCEPT    TES4
070400941028     C                   SETOFF                                       OF18
070500941027     C                   ENDDO
070600900626     C*
070700941027     C                   MOVE      SAVNRS        STANRS            2 0
070800920521     C                   MOVEL     SAVLNP        STALNP            3 0
070900900622     C* STAMPO DETTAGLIO
071000900622     C                   EXCEPT    DET
071100900626     C                   SETON                                        03
071200941027     C*
071300900622     C* REIMPOSTO SCHIERE SPUNTA
071400910320     C                   MOVEA     *BLANKS       CAN
071500900627     C                   MOVEA     *ZEROS        SPU
071600020808     C                   Z-ADD     0             XX
071700900622     C*
071800900622     C                   ENDSR
071900941012     C**************************************************************************
072000941012     C*    STAMPO TOTALI
072100941012     C**************************************************************************
072200920805     C     TOTALI        BEGSR
072300941012     C*
072400920805     C   OF              DO
072500920805     C                   EXCEPT    PIEDI
072600920805     C                   EXCEPT    TES2
072700920805     C                   SETOFF                                       OF
072800920805     C                   END
072900920805     C*
073000920805     C                   EXCEPT    TOT1
073100920805     C*
073200020808     C                   Z-ADD     1             XI
073300020808     C     CPS(XI)       DOWGT     0
073400920805     C   OF              DO
073500920805     C                   EXCEPT    PIEDI
073600920805     C                   EXCEPT    TES2
073700920805     C                   EXCEPT    TOT1
073800920805     C                   SETOFF                                       OF
073900920805     C                   END
074000920805     C*
074100920805     C                   EXCEPT    TOT2
074200020808     C                   ADD       1             XI
074300920805     C                   END
074400920805     C*
074500920805     C                   EXCEPT    TOT3
074600920805     C                   EXCEPT    PIEDI
074700941028     C*
074800920805     C                   ENDSR
074900941020     C**************************************************************************
075000941020     C*    R O U T I N E      I N I Z I A L E
075100941020     C**************************************************************************
075200941020     C     *INZSR        BEGSR
075300941020     C*
075400941020     C     KBRV          KLIST                                                  *
075500941027     C                   KFLD                    DLRNPG                         ..CATEGORIA
075600000125     C                   KFLD                    WNFV                           ..NR.FOGLIO
075700020927     C                   KFLD                    COMfil                         ..FIL.GESTIONE
075800941020     C                   KFLD                    BRVLNA                         ..LNA
075900070112     c*
076000000125     C     KBRV1         KLIST                                                  *
076100000125     C                   KFLD                    DLRNPG                         ..CATEGORIA
076200000125     C                   KFLD                    WNFV                           ..NR.FOGLIO
076300020927     C                   KFLD                    COMfil                         ..FIL.GESTIONE
076400070112     c*
076500941020     C     KBRV2         KLIST                                                  *
076600941027     C                   KFLD                    DLRNPG                         ..CATEGORIA
076700000125     C                   KFLD                    WNFV                           ..NR.FOGLIO
076800020927     C                   KFLD                    COMfil                         ..FIL.GESTIONE
076900941027     C                   KFLD                    DLRLNA                         ..LNA
077000070112     c*
077100941020     C     KFVV          KLIST                                                  *
077200941027     C                   KFLD                    DLRNPG                         ..CATEGORIA
077300941027     C                   KFLD                    DLRNFV                         ..NR.FOGLIO
077400941027     C                   KFLD                    DLRFGS                         ..FIL GEST
077500941027     C     KFVA          KLIST                                                  *
077600941027     C                   KFLD                    DLRFGS                         ..FIL GEST
077700941027     C                   KFLD                    DLRNFV                         ..NR.FOGLIO
077800020808     C     K02FVA01      klist                                                  *
077900020927     C                   kfld                    FVAlnp                         ..P.O.PART
078000020808     C                   kfld                    DLRnfv                         ..NR.FOGLIO
078100020808     C     K02FGV01      klist                                                  *
078200020808     C                   kfld                    DLRnfv                         ..NR.FOGLIO
078300020927     C                   kfld                    FGVlnp                         ..P.O.PART
078400941020     C     KTAB          KLIST                                                  TABEL00F
078500941020     C                   KFLD                    CODUT                          *
078600941020     C                   KFLD                    COD                            *
078700941020     C     KTAB2         KLIST                                                  TABEL00F
078800941020     C                   KFLD                    CODUT                          *
078900941020     C                   KFLD                    COD                            *
079000941020     C                   KFLD                    KEY                            *
079100941020     C*
079200941020     C                   ENDSR
079300941012     C**************************************************************************
079400910325     OQSYSPRT   E            TES2             02
079500910321     O                       SK(1)               66
079600910320     O                       SK(6)              132
079700900626     O                       RSUT                20
079800941028     O                       STACFV              72
079900941028     O                       WUDATE              92 '  .  .    '
080000941020     O                       UTIME              107 '  :  :  '
080100901115     O                       PAGE          Z    132
080200941028     O          E    01      TES2        1
080300941028     O                       SK(14)              66
080400941028     O                       SK(15)             132
080500911030     O          E            TES2        2
080600901114     O                       SK(2)               66
080700901114     O                       SK(4)              132
080800941028     O                       DLRFGS              16
080900920807     O                       DESFEL              39
081000941027     O                       DLRNFV        Z     53
081100941020     O                       STADFV              72 '  /  /    '
081200941028     O                       SAVLNA             102
081300941028     O                                          104 '-'
081400941028     O                       DESLNA             125
081500941028     O          E    17      TES2        2
081600941028     O                     15SK(10)              66
081700161006     O******               15SK(3)              132
081800941028     O                     16DECSPU              12
081900941028     O                     16DESSPU              33
082000161006     O                     15DESINV              70
082100941020     O*------------------------------------
082200941028     O          E            TES3        2
082300941027     O                       SK(13)              66
082400941031     O                       SAVZNC              24
082500941028     O          E   N18      TES3        1
082600941028     O                                           66 ' '
082700941028     O*------------------------------------
082800941028     O          E            TES4        2
082900910321     O                       SK(9)               66
083000941020     O*------------------------------------
083100900626     O          E            DET         1
083200920206     O                    N03STALNP              03
083300920206     O                    N03                    04 '-'
083400920206     O                    N03DESLNP              24
083500920206     O                    N03STANRS              29
083600920206     O                    N03                    31 '-'
083700920206     O                       SPU(1)        Z     39
083800920206     O                       CAN(1)              42
083900920206     O                       SPU(2)        Z     50
084000920206     O                       CAN(2)              53
084100920206     O                       SPU(3)        Z     61
084200920206     O                       CAN(3)              64
084300920206     O                       SPU(4)        Z     72
084400920206     O                       CAN(4)              75
084500920206     O                       SPU(5)        Z     83
084600920206     O                       CAN(5)              86
084700920206     O                       SPU(6)        Z     94
084800920206     O                       CAN(6)              97
084900920206     O                       SPU(7)        Z    105
085000920206     O                       CAN(7)             108
085100920206     O                       SPU(8)        Z    116
085200920206     O                       CAN(8)             119
085300920206     O                       SPU(9)        Z    127
085400920206     O                       CAN(9)             130
085500941020     O*------------------------------------
085600910320     O          E            PIEDI            63
085700910321     O                       SK(8)
085800910321     O          E            PIEDI            64
085900910321     O                       LA1                132
086000910321     O          E            PIEDI            65
086100910321     O                       LA2                132
086200941020     O*------------------------------------
086300920807     O          E            TOT1        3  1
086400910321     O                       SK(7)               66
086500941020     O*------------------------------------
086600920805     O          E            TOT2           1
086700020808     O                       NPS(XI)             33
086800020808     O                       CPS(XI)       2     58
086900941020     O*------------------------------------
087000920805     O          E            TOT3           1
087100920805     O                       SK(11)              66
087200920805     O          E            TOT3           1
087300920805     O                       SK(12)              66
087400920805     O                       TOTSPU        2     58
087500941012     O**************************************************************************
087600941027**
087700090331XXXXXXXXXXXXXXXXXXXX                   ***  STAMPA  SPUNTA  XXXXXX   1
087800941028F O G L I O: XXX - XXXXXXXXXXXXXXXXXXXX   N.RO  XXXXX    DEL  XX/X   2
087900161006
088000941028X/XXXX          LINEA DI ARRIVO:                                     4
088100920805XXX-XXXXXXXXXXXXXXXXXXXX   XX -   XXXXXXXXXX XXXXXXXXXX XXXXXXXXXX   5
088200941028XXXXXX ***      XX/XX/XXXX       XX:XX:XX     FNLR06R     PAG.XXXX   6
088300920805   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI       7
088400920805LEGENDA ANOMALIE:   *=SEGNACOLLO INSERITO MANUALMENTE                8
088500920805LINEA DI PARTENZA         SER                                        9
088600161006                                           INVENTARIO  XXXXXXXXXXxxx10
088700941020                        ---------------------------------------     11
088800941020               TOTALE                                               12
088900941028   * ZONA CONSEGNA :  XX                                            13
089000941028                                           ** SEGNACOLLI CON ANOMA  14
089100941028LIA **                                                              15
089200941020**
089300941027OVRPRTF FILE(QSYSPRT)     USRDTA('Spunte')
089400941027**
089500941027OVRPRTF FILE(QSYSPRT) USRDTA('Spunte an.')
