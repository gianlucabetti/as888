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
006900941027     D DS7J          E DS
007000090331     D DS3E          E DS
007100941027     D KPJBA         E DS
007200900621     D CNCR80        E DS
007300900621     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
007400020927     d OG143         e ds
007500050512     d dfvvspn       e ds
007600020927     d* Parametri per pgm. FNLV55R
007700020927     d FNLV55DS      e ds                  inz
007800020927     d  D55tpt       e                     inz('P')
007900070115     d FnbrvxxDs     e ds                  ExtName(Fnbrv00f)
008000020808
008100020808     D**
008200020808     D* DEFINIZIONE VARIABILI
008300020808     D**
008400020808     D COD             S                   LIKE(TBLCOD)
008500020808     D KEY             S                   LIKE(TBLKEY)
008600020808     D STADFV          S                   LIKE(FVVDFV)                         DATA FOGLIO VIAGGIO
008700020808     D WDFV            S                   LIKE(FVVDFV)                         DATA FOGLIO VIAGGIO
008800020808     D WNFV            S                   LIKE(BRVNFV)                         NUM. FOGLIO VIAGGIO
008900020808     d COMfil          s                   like(FVVfgs) inz
009000070112     d lnapre          s                   like(brvlna) inz
009100020808     D XX              S              3  0 inz
009200020808     D XY              S              2  0 inz
009300020808     D XI              S              3  0 inz
009400020808
009500941028     D**
009600941028     D* DEFINIZIONE COSTANTI
009700941028     D**
009800941028     D CTRAZ           C                   CONST('Trazionista:')
009900941028     D CSPUNT          C                   CONST('Spuntatore :')
010000020808
010100000000     C*---------------------------------------------------------------*
010200900622     C     *ENTRY        PLIST
010300900622     C                   PARM                    KPJBA
010400941027     C                   MOVEL     KPJBU         DSLR01
010500000125     C                   Z-ADD     DLRNFV        WNFV
010600941020     C*
010700000000     C                   Z-ADD     1             CODUT
010800900621     C                   CALL      'X§PARUT'
010900900621     C                   PARM                    UT§DSE
011000941020     C                   MOVEL     RAGUT         RSUT             20
011100900621     C                   MOVEL     REC80         CNCR80
011200900828     C*-----------------------------------------------------
011300941027     C* OPERAZIONI INIZIALI
011400910321     C                   EXSR      INIZIO
011500941027     C*
011600941027     C     *INH1         IFEQ      *OFF
011700020927     c     Wok           andeq     'S'
011800900626     C*
011900941020     C* UNA SOLA LINEA DI ARRIVO ?
012000941027     C     DLRLNA        COMP      0                                  06        ON=UNA SOLA
012100941020     C*
012200070112     c                   if        *in01 = '0'
012300070112     c                   exsr      sr_noanom
012400070112     c                   else
012500070112     c                   exsr      sr_sianom
012600070112     c                   endif
012700070112     c*
012800941027     C                   ENDIF
012900910320     C*
013000000000     C                   SETON                                        LR
013100070112     C**************************************************************************
013200070112     c     sr_noanom     begsr
013300070115     C     KBRV2         SETLL     FNBRV02L
013400070115     C  N06KBRV1         READE     FNBRV02L                               30
013500070115     C   06KBRV2         READE     FNBRV02L                               30
013600070112    1C     *IN30         DOWEQ     *OFF
013700070112     C*
013800070112     C* PULIZIA CAMPI DI TOTALE
013900070112     C                   Z-ADD     0             TOTSPU
014000070112     C                   Z-ADD     1             XY
014100070112     C                   MOVEA     *ZEROS        CPS
014200070112     C                   MOVEL     *BLANKS       NPS
014300070112     C                   Z-ADD     0             XX
014400070112     C*
014500070112    2C     *IN30         DOWEQ     *OFF
014600070112     C*
014700070112     c                   exsr      sr_elabora
014800070112     C*
014900070115     C     KBRV          READE     FNBRV02L                               30
015000070112    2C                   ENDDO
015100070112     C*
015200070112     C* SE CI SONO SPUNTE STAMPO
015300070112    2C     SPU(1)        IFNE      0
015400070112     C                   EXSR      STAMPA
015500070112    2C                   END
015600070112     C* STAMPO TOTALE SPUNTE SE CE NE SONO
015700070112    2C     TOTSPU        IFNE      0
015800070112     C                   EXSR      TOTALI
015900070112    2C                   END
016000070112     C*
016100070112     C* CONTROLLO SE C'E' NUOVA LINEA DI ARRIVO
016200070112     C                   SETOFF                                       020304
016300070112     C*
016400070112     C                   if        *in06 = '0'
016500070115     C     KBRV          SETGT     FNBRV02L
016600070115     C     KBRV1         READE     FNBRV02L                               30
016700070112     c                   endif
016800070112    1C                   ENDDO
016900070112     c                   endsr
017000070112     C**************************************************************************
017100070112     c     sr_sianom     begsr
017200070112     c                   clear                   lnapre
017300070112    1c                   if        dlrlna = 0
017400070112     C/EXEC SQL
017500070112     C+ declare C1 cursor for
017600070115     c+ select * from fnbrv02l where brvnpg = :dlrnpg and
017700070112     c+ brvnfv= :Wnfv and brvfgs= :comfil
017800070112     c+ order by brvnpg, brvnfv, brvfgs, brvlna, brvznc, brvlnp,
017900070112     c+ brvnrs, brvnsc for fetch only
018000070112     C/END-EXEC
018100070112     c* apertura cursore
018200070112     c*
018300070112     C/EXEC SQL
018400070112     C+ Open C1
018500070112     C/END-EXEC
018600070112   x1c                   else
018700070112     C/EXEC SQL
018800070112     C+ declare C2 cursor for
018900070115     c+ select * from fnbrv02l where brvnpg = :dlrnpg and
019000070112     c+ brvnfv= :Wnfv and brvfgs= :comfil and brvlna= :dlrlna
019100070112     c+ order by brvnpg, brvnfv, brvfgs, brvlna, brvznc, brvlnp,
019200070112     c+ brvnrs, brvnsc for fetch only
019300070112     C/END-EXEC
019400070115     C* PULIZIA CAMPI DI TOTALE
019500070115     C                   Z-ADD     0             TOTSPU
019600070115     C                   Z-ADD     1             XY
019700070115     C                   MOVEA     *ZEROS        CPS
019800070115     C                   MOVEL     *BLANKS       NPS
019900070115     C                   Z-ADD     0             XX
020000070112     c* apertura cursore
020100070112     c*
020200070112     C/EXEC SQL
020300070112     C+ Open C2
020400070112     C/END-EXEC
020500070112    1c                   endif
020600070112     c*
020700070112     c*
020800070112     c* lettura cursore
020900070112     c*
021000070112    1c                   do        *hival
021100070112     c                   if        dlrlna = 0
021200070112     C/EXEC SQL
021300070115     C+ Fetch C1 into :fnbrvxxds
021400070112     C/END-EXEC
021500070112     c                   else
021600070112     C/EXEC SQL
021700070115     C+ Fetch C2 into :fnbrvxxds
021800070112     C/END-EXEC
021900070112     c                   endif
022000070112    2c                   select
022100070112     c                   when      sqlcod = 100  or sqlcod < 0
022200070112     c                   leave
022300070115     c                   when      sqlcod = 0
022400070112     c* OPERAZIONI A ROTTURA DI LNA
022500070112     c* se non è stata richiesta una specifica lna a cambio lna devo
022600070112     c* eseguire le operazioni a totale
022700070112    3c                   if        not *in06  and lnapre <> brvlna
022800070112    4c                   if        lnapre > 0
022900070112     C* SE CI SONO SPUNTE STAMPO
023000070112    5C     SPU(1)        IFNE      0
023100070112     C                   EXSR      STAMPA
023200070112    5C                   END
023300070112     C* STAMPO TOTALE SPUNTE SE CE NE SONO
023400070112    5C     TOTSPU        IFNE      0
023500070112     C                   EXSR      TOTALI
023600070112    5C                   END
023700070112    4c                   endif
023800070112     C                   SETOFF                                       020304
023900070112     C* PULIZIA CAMPI DI TOTALE
024000070112     C                   Z-ADD     0             TOTSPU
024100070112     C                   Z-ADD     1             XY
024200070112     C                   MOVEA     *ZEROS        CPS
024300070112     C                   MOVEL     *BLANKS       NPS
024400070112     C                   Z-ADD     0             XX
024500070112     c                   move      brvlna        lnapre
024600070112    3c                   endif
024700070112     c                   exsr      sr_elabora
024800070112    2c                   endsl
024900070112
025000070112    1c                   EndDo
025100070112     C* SE CI SONO SPUNTE STAMPO
025200070112    2C     SPU(1)        IFNE      0
025300070112     C                   EXSR      STAMPA
025400070112    2C                   END
025500070112     C* STAMPO TOTALE SPUNTE SE CE NE SONO
025600070112    2C     TOTSPU        IFNE      0
025700070112     C                   EXSR      TOTALI
025800070112    2C                   END
025900070112     c                   if        dlrlna = 0
026000070112     C/EXEC SQL
026100070112     C+ close C1
026200070112     C/END-EXEC
026300070112     c                   else
026400070112     C/EXEC SQL
026500070112     C+ close C2
026600070112     C/END-EXEC
026700070112     c                   endif
026800070112     c                   endsr
026900070112     C**************************************************************************
027000070112     c     sr_elabora    begsr
027100070112     C* VERIFICO LE ESCLUSIONI
027200070112     C                   EXSR      ESCLUS                                       ESCLUSIONI
027300070112     C*
027400070112    3C     WOK           IFEQ      'S'
027500070112     C*
027600070112     C* IMPOSTO LA LINEA PARTENZA ARRIVO
027700070112     C  N02              EXSR      IMPO
027800070112     C  N02              SETON                                        02
027900070112     C*
028000070115     c                   z-add     brvlnp        brvlnp_d
028100070115     c                   z-add     brvznc        brvznc_d
028200070115     c                   z-add     brvnrs        brvnrs_d
028300070112     C* SE CAMBIA SERIE O LINEA PARTENZA STAMPO RIGA DELLA LINEA
028400070112    4C  N01CONLNP        IFNE      SAVLIN
028500070112     C                   EXSR      CAMLNP
028600070112    4C                   END
028700070112     C*
028800070112    4C   01CONBRV        IFNE      SAVBRV
028900070112     C                   EXSR      CAMLNP
029000070112    4C                   END
029100070112     C**
029200070112     C* CARICO SCHIERE SPUNTA
029300070112     C                   ADD       1             XX
029400070112     C                   ADD       1             TOTSPU            9 0
029500070112     C                   Z-ADD     BRVNSC        SPU(XX)
029600070112     C                   MOVE      BRVCAN        CAN(XX)
029700070112     C*
029800070112     C* SE PISTOLA MANUALE LA INDICO CON L'*
029900070112     C                   MOVEL     ' '           CAN(XX)
030000070112     C     BRVNPS        LOOKUP    NPM                                    31
030100070112    4C     *IN31         IFEQ      *ON
030200070112     C                   MOVEL     '*'           CAN(XX)
030300070112    4C                   END
030400070112     C*
030500070112     C* MEMORIZZO TOTALI PER LETTORE BARCODE
030600070112     C                   Z-ADD     1             XI
030700070112     C                   MOVEL     BRVNPS        COMNPS            2
030800070112     C     COMNPS        LOOKUP    NPS(XI)                                31
030900070112     C*
031000070112     C* 31 OFF - NON ESISTE AGGIUNGO
031100070112     C  N31              MOVEL     COMNPS        NPS(XY)
031200070112     C  N31              Z-ADD     XY            XI
031300070112     C  N31              ADD       1             XY
031400070112     C*
031500070112     C                   ADD       1             CPS(XI)
031600070112     C* SE CARICATE 9 SPUNTE STAMPO RIGA DETTAGLIO
031700070112    4C     XX            IFEQ      9
031800070112     C                   EXSR      STAMPA
031900070112    4C                   END
032000070112     C*
032100070112    3C                   ENDIF                                                  WOK = 'S'
032200070112     c                   endsr
032300941020     C**************************************************************************
032400941020     C*   ESCLUSIONI DURANTE LE LETTURE
032500941020     C**************************************************************************
032600941020     C     ESCLUS        BEGSR
032700941020     C*
032800941020     C                   MOVEL     'S'           WOK               1            DI COMODO
032900941020     C*
033000941020     C* NR.PISTOLA SPUNTA-COLLI
033100941027     C     BRVNPS        IFLT      DLRNPD                                       PISTOLA INIZIALE
033200941027     C     BRVNPS        ORGT      DLRNPA                                       PISTOLA FINALE
033300941020     C                   MOVEL     *BLANKS       WOK                            DA ESCLUDERE
033400941020     C                   ENDIF
033500941020     C*
033600941020     C* SERIE
033700941027     C     BRVNRS        IFLT      DLRSRD                                       SERIE INIZIALE
033800941027     C     BRVNRS        ORGT      DLRSRA                                       SERIE FINALE
033900941020     C                   MOVEL     *BLANKS       WOK                            DA ESCLUDERE
034000941020     C                   ENDIF
034100941027     C*
034200941027     C* SE SEGNACOLLI CON ANOMALIA ESCLUDO SE NON HA ANOMALIA
034300941027     C   01BRVCAN        IFEQ      ' '
034400941027     C                   MOVEL     *BLANKS       WOK                            DA ESCLUDERE
034500941027     C                   ENDIF
034600941020     C*
034700941020     C                   ENDSR
034800941012     C**************************************************************************
034900941027     C*   O P E R A Z I O N I    I N I Z I A L I
035000941027     C*
035100941027     C*   RICHIAMO DEL FOGLIO VIAGGIO DA STAMPARE:
035200941027     C*  ?  .. SU FNFGV  <---  SE CATEGORIA = 1 & LNP=SIMFEL     ?
035300941027     C*  ?  .. SU FNFVA  <---  SE CATEGORIA = 1 & LNP NON IN £1  ?
035400941027     C*  ?  .. SU FNFVV  <---  SE CATEGORIA > 1                  ?
035500941012     C**************************************************************************
035600910321     C     INIZIO        BEGSR
035700020927     c*
035800020927     c                   move      *date         G02DAT
035900020927     c                   clear                   G02ERR
036000020927     c                   call      'XSRDA8'
036100020927     c                   parm                    WLBDAT
036200020927     c                   z-add     G02inv        WDATEU            8 0          AAAA/MM/GG
036300941027     C***
036400941027     C* CARICO TABELLA FILIALI GESTITE £1
036500941027     C***
036600941027     C                   CLEAR                   DSUL06
036700941027     C                   MOVE      '£1'          D06COD
036800941027     C                   MOVEL     SIMFEL        D06KEY
036900941027     C                   MOVEL     'L'           D06TLA
037000941027     C                   MOVEL     DSUL06        KPJBU
037100941027     C*
037200941027     C                   CALL      'TRUL06R'
037300941027     C                   PARM                    KPJBA
037400941027     C                   MOVEL     KPJBU         DSUL06
037500941027     C                   MOVEA     LIN           L1
037600020808     C**
037700020808     C* DECODIFICA FIL GESTIONE
037800020808     C**
037900020808     C     DLRFGS        CHAIN     AZORG01L                           31
038000020808     C                   MOVEL     ORGDES        DESFEL           20
038100020927     c                   movel     ORGde3        OG143
038200020927     c                   reset                   FNLV55ds
038300020927     c                   if        §OGntw = 'PPT'
038400020927     c                   eval      D55lin = DLRfgs
038500020927     c                   eval      D55drf = WDATEU
038600020927     c                   call      'FNLV55R'
038700020927     c                   parm                    FNLV55DS
038800020927     c***                if        D55err <> *blanks
038900020927     c***                eval      D55tfp =  D55lin
039000020927     c***                endif
039100020927     c                   endif
039200020927     c                   if        §OGntw = 'PPT'
039300020927     c                             and DLRnpg = 1
039400020927     c                   z-add     D55tfp        COMfil
039500020927     c                   else
039600020927     c                   z-add     DLRfgs        COMfil
039700020927     c                   endif
039800020927     c*
039900020927     c                   eval      wOK    = 'S'
040000941027     C**
040100941027     C* CHAIN FOGLIO
040200941027     C**
040300941027     C                   CLEAR                   WDFV
040400941027     C*
040500941027    1C     DLRNPG        IFEQ      1
040600941027     C*
040700941027     C     DLRFGS        LOOKUP    L1                                     30
040800941027     C*
040900941027    2C     *IN30         IFEQ      *ON
041000020808     ***C     DLRNFV        CHAIN     FNFGV000                           31
041100020927     c                   eval      FGVlnp = DLRfgs
041200020927      * se p.o. poste reperisco il p.o. di partenza
041300020927    3c                   if        §OGntw = 'PPT'
041400020927     c     KFVA          chain     FNFGW02L                           31
041500020927     c  N31FGWatb        comp      *blanks                            3131
041600020927     c  N31              eval      FGVlnp = FGWlnp
041700020927     c   31              eval      Wok    = *blanks
041800020927    3c                   endif
041900020808     C     K02FGV01      CHAIN     FNFGV000                           31
042000020927      * se p.o. non poste controllo che il p.o. gestione non sia poste
042100020927    3c                   if        not *in31
042200020927     c                             and §OGntw <> 'PPT'
042300020927     c     K02FGV01      chain     FNFGW01L
042400020927    4c                   if        %found(FNFGW01L)
042500020927     c                             and FGWatb =  *blanks
042600020927     c                             and FGWfgs <> *zeros
042700020927     c                             and FGWfgs <> FGVlnp
042800020927     c                   eval      *in31 = *on
042900020927    4c                   endif
043000020927    3c                   endif
043100020927     c   31              eval      Wok    = *blanks
043200941028     C*
043300941028     C     *IN31         IFEQ      *OFF
043400941028     C                   MOVEL     FGVDFV        WDFV
043500941028     C* IMPOSTO NELLA STAMPA IL TRASIONISTA
043600941028     C     FGVDPD        IFNE      *BLANKS
043700941028     C                   MOVEL     CTRAZ         DECSPU           12
043800941028     C                   MOVEL     FGVDPD        DESSPU           20
043900941028     C                   SETON                                        1617
044000941028     C                   ENDIF
044100941028     C                   ENDIF
044200941028     C*
044300941027   X2C                   ELSE
044400941027     C*
044500941027     C* FOGLIO NON DELL'AREA : CHAIN SU FNFVA
044600020808     ***C     KFVA          CHAIN     FNFVA000                           31
044700020808     ***C  N31              MOVEL     FVADFV        WDFV
044800020927     c                   eval      FVAlnp = DLRfgs
044900020927      * se p.o. poste reperisco il p.o. di partenza
045000020927    3c                   if        §OGntw = 'PPT'
045100020927     c     KFVA          chain     FNFWA02L                           31
045200020927     c  N31FWAatb        comp      *blanks                            3131
045300020927     c  N31              eval      FVAlnp = FWAlnp
045400020927     c   31              eval      Wok    = *blanks
045500020927    3c                   endif
045600020808     c     K02FVA01      CHAIN     FNFVA000                           31
045700020927      * se p.o. non poste controllo che il p.o. gestione non sia poste
045800020927    3c                   if        not *in31
045900020927     c                             and §OGntw <> 'PPT'
046000020927     c     K02FVA01      chain     FNFWA01L
046100020927    4c                   if        %found(FNFWA01L)
046200020927     c                             and FWAatb =  *blanks
046300020927     c                             and FWAfgs <> *zeros
046400020927     c                             and FWAfgs <> FVAlnp
046500020927     c                   eval      *in31 = *on
046600020927    4c                   endif
046700020927    3c                   endif
046800020927     c   31              eval      Wok    = *blanks
046900020927     c*
047000020808    3c     *IN31         IFEQ      *OFF
047100020808     c                   MOVEL     FVADFV        WDFV
047200020927     c* IMPOSTO NELLA STAMPA IL TRAZIONISTA
047300020927     c     FVADPD        IFNE      *BLANKS
047400020927     c                   MOVEL     CTRAZ         DECSPU
047500020927     c                   MOVEL     FVADPD        DESSPU
047600020927     c                   SETON                                        1617
047700020927     c                   ENDIF
047800020808    3c                   ENDIF
047900941027    2C                   ENDIF
048000941027     C*
048100941027   X1C                   ELSE
048200941027     C* NON E' FOGLIO PARTENZA
048300941027     C     KFVV          CHAIN     FNFVV01L                           31
048400941028     C*
048500941028     C     *IN31         IFEQ      *OFF
048600941028     C                   MOVEL     FVVDFV        WDFV
048700050512     C                   MOVEL     FVVSPN        dfvvspn
048800050512     C     §fvvspn       IFNE      *BLANKS
048900941028     C                   MOVEL     CSPUNT        DECSPU
049000050512     C                   MOVEL(P)  §fvvspn       DESSPU
049100941028     C                   SETON                                        1617
049200941028     C                   ENDIF
049300941028     C*
049400941028     C* SE CATEORIGA INVENTARIO DECOCIFICO ANCHE LA SOTTO CATEGORIA
049500941028     C     FVVSPG        IFNE      ' '
049600941028     C                   SETON                                        1517
049700941028     C*
049800941028     C                   MOVEL     '7B'          COD
049900941028     C                   MOVEL(P)  FVVSPG        KEY
050000941028     C     KTAB2         CHAIN     TABEL                              30
050100941028     C  N30              MOVEL     TBLUNI        DESINV           10
050200941028     C   30              MOVEL     *BLANKS       DESINV
050300941028     C                   END
050400941027    1C                   ENDIF
050500941028     C                   ENDIF
050600020927     ***C**
050700020927     ***C* DECODIFICA FIL GESTIONE
050800020927     ***C**
050900020927     ***C     DLRFGS        CHAIN     AZORG01L                           31
051000020927     ***C                   MOVEL     ORGDES        DESFEL           20
051100020927     *** * >>>>> spostata prima <<<<<
051200941027     C**
051300941027     C* DECODIFICA CATEGORIA FOGLIO
051400941027     C**
051500920807     C                   MOVEL     '7N'          COD
051600941027     C                   MOVEL(P)  DLRNPG        KEY
051700920807     C     KTAB2         CHAIN     TABEL                              31
051800920807     C  N31              MOVEL     TBLUNI        STACFV
051900941028     C   31              MOVEL     *BLANKS       STACFV           12
052000920807     C*
052100941027     C**
052200911030     C* DATA FOGLIO VIAGGIO
052300941027     C**
052400941027     C                   Z-ADD     WDFV          G02INV                         AAAA/MM/GG
052500941020     C                   Z-ADD     *ZEROS        G02DAT
052600941020     C                   MOVEL     '3'           G02ERR
052700941020     C                   CALL      'XSRDA8'
052800911030     C                   PARM                    WLBDAT
052900941020     C                   Z-ADD     G02DAT        STADFV                         GG/MM/AAAA
053000910321     C*
053100910321     C* PRENDO FLAG ANOMALIA PRIMI 8
053200090331     c* prendo solo i codici ancora utilizzabili
053300910321     C                   MOVEL     '3E'          COD
053400910321     C                   MOVEL     '='           UGUA
053500020808     C                   Z-ADD     1             XI
053600910321     C     KTAB          SETLL     TABEL                                  31
053700020808     C   31XI            DOWLE     8
053800910321     C     KTAB          READE     TABEL                                  31
053900910321     C  N31              DO
054000910321     C                   MOVEL     TBLKEY        FL3E
054100910321     C                   MOVEL     TBLUNI        DE3E
054200090331     C                   MOVEL     TBLUNI        Ds3e
054300090331     c                   if        §3euti<>'N'
054400090331     c* e e' cat 8 carico tutti altrimenti solo quelli non per 8
054500090331     c                   if        (dlrnpg<>8 and §3ecat8=' ') or
054600090331     c                              dlrnpg=8
054700020808     C                   MOVEL     COMCAN        LA1(XI)
054800020808     C                   ADD       1             XI
054900090331     c                   endif
055000090331     c                   endif
055100090331     c
055200090331     C                   ENDDO
055300910321     C* FINE LETTURA
055400020808     C   31              Z-ADD     20            XI
055500910321     C                   END
055600020808     C  N31              Z-ADD     1             XI
055700910321     C*
055800910321     C* PRENDO FLAG ANOMALIA DA 9 A 16
055900020808     C     XI            DOWLE     8
056000910321     C     KTAB          READE     TABEL                                  31
056100910321     C  N31              DO
056200910321     C                   MOVEL     TBLKEY        FL3E
056300910321     C                   MOVEL     TBLUNI        DE3E
056400090331     C                   MOVEL     TBLUNI        Ds3e
056500090331     c                   if        §3euti<>'N'
056600090331     c* e e' cat 8 carico tutti altrimenti solo quelli non per 8
056700090331     c                   if        (dlrnpg<>8 and §3ecat8=' ') or
056800090331     c                              dlrnpg=8
056900020808     C                   MOVEL     COMCAN        LA2(XI)
057000020808     C                   ADD       1             XI
057100090331     c                   endif
057200090331     c                   endif
057300910321     C                   END
057400910321     C* FINE LETTURA
057500020808     C   31              Z-ADD     20            XI
057600910321     C                   END
057700910321     C*
057800941020     C* RILEVO LA DATA E L'ORA PER LA STAMPA
057900941020     C                   TIME                    WTIME            14 0          ORA E DATA
058000941020     C                   MOVE      WTIME         WUDATE            8 0          GG/MM/AAAA
058100941020     C                   MOVEL     WTIME         UTIME             6 0          ORARIO
058200910321     C                   Z-ADD     0             BRVLNA
058300941027     C***
058400941027     C* CARICO LE PISTOLE MANUALI
058500941027     C***
058600941027     C                   MOVEL     '7J'          COD
058700020808     C                   Z-ADD     1             XI
058800941027     C     KTAB          SETLL     TABEL
058900941027     C     KTAB          READE     TABEL                                  30
059000941027     C*
059100941027     C     *IN30         DOWEQ     *OFF
059200941027     C     TBLFLG        IFEQ      ' '
059300941027     C                   MOVEL     TBLUNI        DS7J
059400941027     C* SE MANUALE LA MEMORIZZO
059500941027     C     §7JRPS        IFEQ      'M'
059600020808     C                   MOVEL     TBLKEY        NPM(XI)
059700020808     C                   ADD       1             XI
059800941027     C                   ENDIF
059900941027     C                   ENDIF
060000941027     C*
060100941027     C     KTAB          READE     TABEL                                  30
060200941027     C                   ENDDO
060300941027     C**
060400941027     C* DLRFS1 = 'S' STAMPA SPUNTA = 'A' STAMPA SPUNTE CON ANOMALIA
060500941027     C**
060600941027     C     DLRFS1        IFEQ      'A'
060700941027     C                   SETON                                        01
060800941027     C                   ENDIF
060900941027     C**
061000941027     C* PREPARO STAMPANTE
061100941027     C**
061200941027     C                   Z-ADD     80            LUNG             15 5
061300941027     C                   MOVEL     *BLANKS       COMMAN           80
061400941027     C  N01              MOVEA     CMD           COMMAN
061500941027     C   01              MOVEA     CM2           COMMAN
061600941027     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
061700941027     C                   PARM                    COMMAN
061800941027     C                   PARM                    LUNG
061900941027     C*
062000941027     C  NH1              OPEN      QSYSPRT
062100910321     C*
062200910321     C                   ENDSR
062300941012     C**************************************************************************
062400941012     C*    CAMBIA LINEA PARTENZA
062500941012     C**************************************************************************
062600900626     C     CAMLNP        BEGSR
062700941012     C*
062800900626     C* SE CI SONO SPUNTE STAMPO
062900900626     C     SPU(1)        IFNE      0
063000900626     C                   EXSR      STAMPA
063100900626     C                   END
063200941031     C*
063300941031     C* PER SEGNACOLLI CON ANOMALIA, SE NON ACCESO OF STAMPO LA NUOVA
063400941031     C* ZONA
063500941031     C     *IN01         IFEQ      *ON
063600941031     C     *INOF         ANDEQ     *OFF
063700941031     C     BRVZNC        ANDNE     SAVZNC
063800941031     C                   MOVE      BRVZNC        SAVZNC
063900941031     C                   EXCEPT    TES3
064000941031     C                   ENDIF
064100941031     C* SALVO NUOVA LINEA
064200941031     C                   MOVEL     BRVLNP        SAVLNP
064300941031     C                   MOVE      BRVNRS        SAVNRS
064400941031     C                   MOVE      BRVZNC        SAVZNC
064500900626     C*
064600900626     C* DECODIFICO LINEA PARTENZA
064700941012     C     BRVLNP        CHAIN     AZORG01L                           31
064800920206     C  N31              MOVEL     ORGDES        DESLNP           20
064900900626     C   31              MOVEL     *BLANKS       DESLNP
065000900626     C* STAMPO TESTATA
065100900626     C                   SETOFF                                       03
065200900626     C                   ENDSR
065300941012     C**************************************************************************
065400941012     C* IMPOSTAZIONE INIZIALE
065500941012     C**************************************************************************
065600900621     C     IMPO          BEGSR
065700941012     C*
065800920521     C* SALVO PRIMA LINEA
065900920521     C                   Z-ADD     BRVLNA        SAVLNA            3 0
066000941012     C     SAVLNA        CHAIN     AZORG01L                           32
066100920521     C  N32              MOVEL     ORGDES        DESLNA           20
066200920521     C   32              MOVEL     *BLANKS       DESLNA
066300941027     C*
066400900626     C* SALVO PRIMA LINEA PARTENZA E SERIE
066500941027     C                   MOVEL     BRVLNP        SAVLNP
066600941027     C                   MOVE      BRVNRS        SAVNRS
066700941027     C                   MOVE      BRVZNC        SAVZNC
066800941027     C*
066900900621     C* DECODIFICO LINEA PARTENZA
067000941012     C     BRVLNP        CHAIN     AZORG01L                           31
067100920206     C  N31              MOVEL     ORGDES        DESLNP           20
067200900622     C   31              MOVEL     *BLANKS       DESLNP
067300900626     C*
067400941027     C* TESTATA SOLO PER STAMPA SPUNTE
067500941027     C     *IN01         IFEQ      *OFF
067600901114     C                   EXCEPT    TES2
067700941028     C                   EXCEPT    TES4
067800941028     C                   SETON                                        04
067900941027     C                   ELSE
068000941027     C                   SETON                                        OF
068100941027     C                   ENDIF
068200900626     C*
068300900621     C                   ENDSR
068400941012     C**************************************************************************
068500941012     C* STAMPO SPUNTA
068600941012     C**************************************************************************
068700900621     C     STAMPA        BEGSR
068800941012     C*
068900941027     C* STAMPO TESTATA
069000900622     C   OF              DO
069100941027     C**
069200941027     C* LA PRIMA VOLTA NON STAMPO I PIEDI
069300941028     C     *IN04         IFEQ      *ON
069400901114     C                   EXCEPT    PIEDI
069500941027     C                   ENDIF
069600941027     C**
069700941027     C                   SETOFF                                       03
069800941028     C                   SETON                                        0418
069900901114     C                   EXCEPT    TES2
070000941028     C   01              EXCEPT    TES3
070100941028     C                   EXCEPT    TES4
070200941028     C                   SETOFF                                       OF18
070300941027     C                   ENDDO
070400900626     C*
070500941027     C                   MOVE      SAVNRS        STANRS            2 0
070600920521     C                   MOVEL     SAVLNP        STALNP            3 0
070700900622     C* STAMPO DETTAGLIO
070800900622     C                   EXCEPT    DET
070900900626     C                   SETON                                        03
071000941027     C*
071100900622     C* REIMPOSTO SCHIERE SPUNTA
071200910320     C                   MOVEA     *BLANKS       CAN
071300900627     C                   MOVEA     *ZEROS        SPU
071400020808     C                   Z-ADD     0             XX
071500900622     C*
071600900622     C                   ENDSR
071700941012     C**************************************************************************
071800941012     C*    STAMPO TOTALI
071900941012     C**************************************************************************
072000920805     C     TOTALI        BEGSR
072100941012     C*
072200920805     C   OF              DO
072300920805     C                   EXCEPT    PIEDI
072400920805     C                   EXCEPT    TES2
072500920805     C                   SETOFF                                       OF
072600920805     C                   END
072700920805     C*
072800920805     C                   EXCEPT    TOT1
072900920805     C*
073000020808     C                   Z-ADD     1             XI
073100020808     C     CPS(XI)       DOWGT     0
073200920805     C   OF              DO
073300920805     C                   EXCEPT    PIEDI
073400920805     C                   EXCEPT    TES2
073500920805     C                   EXCEPT    TOT1
073600920805     C                   SETOFF                                       OF
073700920805     C                   END
073800920805     C*
073900920805     C                   EXCEPT    TOT2
074000020808     C                   ADD       1             XI
074100920805     C                   END
074200920805     C*
074300920805     C                   EXCEPT    TOT3
074400920805     C                   EXCEPT    PIEDI
074500941028     C*
074600920805     C                   ENDSR
074700941020     C**************************************************************************
074800941020     C*    R O U T I N E      I N I Z I A L E
074900941020     C**************************************************************************
075000941020     C     *INZSR        BEGSR
075100941020     C*
075200941020     C     KBRV          KLIST                                                  *
075300941027     C                   KFLD                    DLRNPG                         ..CATEGORIA
075400000125     C                   KFLD                    WNFV                           ..NR.FOGLIO
075500020927     C                   KFLD                    COMfil                         ..FIL.GESTIONE
075600941020     C                   KFLD                    BRVLNA                         ..LNA
075700070112     c*
075800000125     C     KBRV1         KLIST                                                  *
075900000125     C                   KFLD                    DLRNPG                         ..CATEGORIA
076000000125     C                   KFLD                    WNFV                           ..NR.FOGLIO
076100020927     C                   KFLD                    COMfil                         ..FIL.GESTIONE
076200070112     c*
076300941020     C     KBRV2         KLIST                                                  *
076400941027     C                   KFLD                    DLRNPG                         ..CATEGORIA
076500000125     C                   KFLD                    WNFV                           ..NR.FOGLIO
076600020927     C                   KFLD                    COMfil                         ..FIL.GESTIONE
076700941027     C                   KFLD                    DLRLNA                         ..LNA
076800070112     c*
076900941020     C     KFVV          KLIST                                                  *
077000941027     C                   KFLD                    DLRNPG                         ..CATEGORIA
077100941027     C                   KFLD                    DLRNFV                         ..NR.FOGLIO
077200941027     C                   KFLD                    DLRFGS                         ..FIL GEST
077300941027     C     KFVA          KLIST                                                  *
077400941027     C                   KFLD                    DLRFGS                         ..FIL GEST
077500941027     C                   KFLD                    DLRNFV                         ..NR.FOGLIO
077600020808     C     K02FVA01      klist                                                  *
077700020927     C                   kfld                    FVAlnp                         ..P.O.PART
077800020808     C                   kfld                    DLRnfv                         ..NR.FOGLIO
077900020808     C     K02FGV01      klist                                                  *
078000020808     C                   kfld                    DLRnfv                         ..NR.FOGLIO
078100020927     C                   kfld                    FGVlnp                         ..P.O.PART
078200941020     C     KTAB          KLIST                                                  TABEL00F
078300941020     C                   KFLD                    CODUT                          *
078400941020     C                   KFLD                    COD                            *
078500941020     C     KTAB2         KLIST                                                  TABEL00F
078600941020     C                   KFLD                    CODUT                          *
078700941020     C                   KFLD                    COD                            *
078800941020     C                   KFLD                    KEY                            *
078900941020     C*
079000941020     C                   ENDSR
079100941012     C**************************************************************************
079200910325     OQSYSPRT   E            TES2             02
079300910321     O                       SK(1)               66
079400910320     O                       SK(6)              132
079500900626     O                       RSUT                20
079600941028     O                       STACFV              72
079700941028     O                       WUDATE              92 '  .  .    '
079800941020     O                       UTIME              107 '  :  :  '
079900901115     O                       PAGE          Z    132
080000941028     O          E    01      TES2        1
080100941028     O                       SK(14)              66
080200941028     O                       SK(15)             132
080300911030     O          E            TES2        2
080400901114     O                       SK(2)               66
080500901114     O                       SK(4)              132
080600941028     O                       DLRFGS              16
080700920807     O                       DESFEL              39
080800941027     O                       DLRNFV        Z     53
080900941020     O                       STADFV              72 '  /  /    '
081000941028     O                       SAVLNA             102
081100941028     O                                          104 '-'
081200941028     O                       DESLNA             125
081300941028     O          E    17      TES2        2
081400941028     O                     15SK(10)              66
081500941028     O                     15SK(3)              132
081600941028     O                     16DECSPU              12
081700941028     O                     16DESSPU              33
081800941028     O                     15DESINV              64
081900941028     O                     15FVVSPG              75
082000941020     O*------------------------------------
082100941028     O          E            TES3        2
082200941027     O                       SK(13)              66
082300941031     O                       SAVZNC              24
082400941028     O          E   N18      TES3        1
082500941028     O                                           66 ' '
082600941028     O*------------------------------------
082700941028     O          E            TES4        2
082800910321     O                       SK(9)               66
082900941020     O*------------------------------------
083000900626     O          E            DET         1
083100920206     O                    N03STALNP              03
083200920206     O                    N03                    04 '-'
083300920206     O                    N03DESLNP              24
083400920206     O                    N03STANRS              29
083500920206     O                    N03                    31 '-'
083600920206     O                       SPU(1)        Z     39
083700920206     O                       CAN(1)              42
083800920206     O                       SPU(2)        Z     50
083900920206     O                       CAN(2)              53
084000920206     O                       SPU(3)        Z     61
084100920206     O                       CAN(3)              64
084200920206     O                       SPU(4)        Z     72
084300920206     O                       CAN(4)              75
084400920206     O                       SPU(5)        Z     83
084500920206     O                       CAN(5)              86
084600920206     O                       SPU(6)        Z     94
084700920206     O                       CAN(6)              97
084800920206     O                       SPU(7)        Z    105
084900920206     O                       CAN(7)             108
085000920206     O                       SPU(8)        Z    116
085100920206     O                       CAN(8)             119
085200920206     O                       SPU(9)        Z    127
085300920206     O                       CAN(9)             130
085400941020     O*------------------------------------
085500910320     O          E            PIEDI            63
085600910321     O                       SK(8)
085700910321     O          E            PIEDI            64
085800910321     O                       LA1                132
085900910321     O          E            PIEDI            65
086000910321     O                       LA2                132
086100941020     O*------------------------------------
086200920807     O          E            TOT1        3  1
086300910321     O                       SK(7)               66
086400941020     O*------------------------------------
086500920805     O          E            TOT2           1
086600020808     O                       NPS(XI)             33
086700020808     O                       CPS(XI)       2     58
086800941020     O*------------------------------------
086900920805     O          E            TOT3           1
087000920805     O                       SK(11)              66
087100920805     O          E            TOT3           1
087200920805     O                       SK(12)              66
087300920805     O                       TOTSPU        2     58
087400941012     O**************************************************************************
087500941027**
087600090331XXXXXXXXXXXXXXXXXXXX                   ***  STAMPA  SPUNTA  XXXXXX   1
087700941028F O G L I O: XXX - XXXXXXXXXXXXXXXXXXXX   N.RO  XXXXX    DEL  XX/X   2
087800941028  I. M. X.                                                           3
087900941028X/XXXX          LINEA DI ARRIVO:                                     4
088000920805XXX-XXXXXXXXXXXXXXXXXXXX   XX -   XXXXXXXXXX XXXXXXXXXX XXXXXXXXXX   5
088100941028XXXXXX ***      XX/XX/XXXX       XX:XX:XX     FNLR06R     PAG.XXXX   6
088200920805   * RIEPILOGO SPUNTE :  LETTORE BARCODE        COLLI SPUNTATI       7
088300920805LEGENDA ANOMALIE:   *=SEGNACOLLO INSERITO MANUALMENTE                8
088400920805LINEA DI PARTENZA         SER                                        9
088500941028                                          INVENTARIO  XXXXXXXXXX :  10
088600941020                        ---------------------------------------     11
088700941020               TOTALE                                               12
088800941028   * ZONA CONSEGNA :  XX                                            13
088900941028                                           ** SEGNACOLLI CON ANOMA  14
089000941028LIA **                                                              15
089100941020**
089200941027OVRPRTF FILE(QSYSPRT)     USRDTA('Spunte')
089300941027**
089400941027OVRPRTF FILE(QSYSPRT) USRDTA('Spunte an.')
