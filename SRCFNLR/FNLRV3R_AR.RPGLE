000100040609     H DECEDIT('0,') DATEDIT(*yMd/)
000200040607     H* FNLRV3R *-----------------------------------------------------*
000300040607     H*   Controllo colli di valore al 2 livello in arrivo - BATCH
000400930126     H*---------------------------------------------------------------*
000500040610     FFNANM02L  UF A E           K DISK
000600040607     Ftabel00f  iF   E           K DISK
000700040607     Fazcae02l  iF   E           K DISK
000800040607     FFNfgv02l  IF   E           K DISK
000900040607     FFNfvv07l  IF   E           K DISK
001000040610     FFNFVV01L  IF   E           K DISK
001100040608     F                                     RENAME(FNfvv000:FNfvv001)
001200070117     FFNBRV02L  IF   E           K DISK
001300040608     f                                     prefix(TER_)
001400070117     FFNBRV07L  IF   E           K DISK
001500070117     F                                     RENAME(Fnbrv000:Fnbrv007)
001600040607     f*
001700040608     Ffnblp01l  IF   E           K DISK
001900090309     Ffiart27l  IF   E           K DISK
002000040608     FAZORG01L  IF   E           K DISK
002100040608     FfIAR501l  IF   E           K DISK
002200040604     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002300040604     F                                     USROPN
002400951114     D*
002500040607     D* passaggio dati a batch - FNLRV3R -
002600040607     D*
002700040607     D PARA02          DS
002800040607     D  P02dam                 1      8  0
002900040607     D  P02fgs                 9     11  0
003000040607     D  P02invdam             12     19  0
003100040609     D*** PARAMETRI PER RICERCA FASE ANOMALIA    - TRUL11R -
003200040609     D PARAM5          DS
003300040609     D  PA5TLA                 1      1
003400040609     D  PA5NPG                 2      2
003500040609     D  PA5SPG                 3      3
003600040609     D  PA5FAS                 4      4
003700951115     D*
003800911010     D WLBDAT          DS
003900941221     D  G02DAT                 1      8  0
004000941221     D  G02INV                 9     16  0
004100941221     D  G02ERR                17     17
004200941221     D  G02TGI                18     22  0
004300960304     D WGIDAT          DS                  INZ
004400960304     D  GIODAT                 1      8  0
004500960304     D  GIOINV                 9     16  0
004600960304     D  GIOTGI                17     21  0
004700040604     D                 DS
004800040604     D  BLPAAS                 1      4  0
004900040604     D  BLPLNP                 5      7  0
005000040604     D  BLPNRS                 8      9  0
005100040604     D  BLPNSP                10     16  0
005200040604     D  BLPSPE                 1     16  0
005300040604     D                 DS
005400040604     D  COMAAS                 1      4  0
005500040604     D  BLPMGS                 5      8  0
005600040604     D  BLPDSP                 1      8  0
005700040604     D
005800040610     D STA             S             66    DIM(16) CTDATA PERRCD(1)             SKIERE STAMPA
005900040604     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
006000040609     D TDEF            S              1    DIM(20)                              FV GIA' SELEZ
006100960304     D*
006200040604     D CNCR80        E DS
006300040604     D og143         E DS
006400040604     D KPJBA         E DS
006500030722     D UT§DSE0F      E DS
006600040608     D FNLV53DS      E DS
006700040607     D DSTV          E DS
006800040609     D DS7c          E DS
006900040608     D DAR5GEN       E DS
007000070117     D RECBRV        E DS                  EXTNAME(FNBRV00F)
007100070117     D WSAVB         E DS                  EXTNAME(FNBRV00F)
007200040608     d                                     prefix(S_)
007300040608     D*
007400040608     D* DS PER TRUL06R - CARICAMENTO £X
007500040608     D DSUL06        E DS                  EXTNAME(TRUL06DS)
007600040608     D  LIN                    1     90  0
007700040608     D                                     DIM(30)                              FIL. COMODO
007800040608     D L6              S              3  0 DIM(30)                              F. MEC    GES
007900040607     d KEPA            s                   Like(CAEEPA) INZ('A')
008000040607     d KCOD            s                   Like(tblcod) INZ('TV')
008100040608     d Kkey            s                   Like(tblkey)
008200040607     d Knpg            s                   Like(brvnpg)
008300040607     d Knfv            s                   Like(brvnfv)
008400040607     d Kfgs            s                   Like(brvfgs)
008500040607     d Klna            s                   Like(brvlna)
008600040609     d wdataspu        s                   Like(d53dfv)
008700040608     d wdfv            S                   LIKE(d53dfv)
008800040608     d wimm            S                   LIKE(brvimm)
008900040608     d WSPG            S                   LIKE(FVVSPG)
009000040608     d ktrd            S                   LIKE(ar5trd) inz('GEN')
009100040609     d comcaa          S                   LIKE(anmcaa) inz(015)
009200040609     d comspe          s                   Like(blpspe)
009300090309     d welalna         s                   Like(artlna)
009400040609     d Wmanca          s              1
009500040610     d WCREa           s              1
009600000000     C*---------------------------------------------------------------*
009700911010     C* INDICATORI
009800911010     C*---------------------------------------------------------------*
009900040608     c*  04   -
010000040608     c*  05   -
010100040608     c*  13   - trovata spunta di altro p.o.
010200911010     C*---------------------------------------------------------------*
010300040607     c* Apro file di stampa
010400040607     C                   Z-ADD     80            LUNG             15 5
010500040607     C                   MOVEL     *BLANKS       COMMAN           80
010600040607     C                   MOVEA     CMD           COMMAN
010700040607     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
010800040607     C                   PARM                    COMMAN
010900040607     C                   PARM                    LUNG
011000040607     C*
011100040607RM*  C                   OPEN      PRTF198
011200040607     C                   SETON                                        OF
011300040608     C*
011400040608     c* carico £6 del p.o. gestione
011500040608     C                   CLEAR                   DSUL06
011600040608     C                   MOVE      '£6'          D06COD
011700040608     C                   MOVEL     p02FGS        D06KEY
011800040608     C                   MOVEL     'L'           D06TLA
011900040608     C                   MOVEL     DSUL06        KPJBU
012000040608     C*
012100040608     C                   CALL      'TRUL06R'
012200040608     C                   PARM                    KPJBA
012300040608     C                   MOVEL     KPJBU         DSUL06
012400040608     C                   MOVEA     LIN           L6
012500040608     c*
012600040608     c* Decodifico p.o. richiesto
012700040608     c     p02fgs        chain     azorg01l
012800040608     c                   if        %found(azorg01l)
012900040608     c                   movel     orgdes        desfgs
013000040608     c                   else
013100040608     c                   clear                   desfgs           20
013200040608     c                   endif
013300040608     c
013400040608     c* elaboro tutti i p.o. della £6
013500040608     c                   z-add     1             yy                3 0
013600040608     c                   dow       l6(yy)>0
013700040608     c                   eval      Welalna=l6(yy)
013800040607     C*
013900040607     c* Lettura di azcae per prendere i terminal di arrivo
014000040607     C     KCAE          SETLL     AZCAE02L
014100040607     C     KCAE          READE     AZCAE02L
014200040607     c
014300040607     C                   DOW       NOT %EOF(azcae02l)
014400040607     c* controllo decorrenza/scadenza
014500040607     c                   if        caeatb=' '  and
014600040607     c                             caedde<=p02invdam and caedsc>=p02invdam
014700040611     c*
014800040611     c* Escludo  se stesso come terminal non ha senso controllare
014900040611     c                   if        caetfe<>p02fgs
015000040607     c
015100040607     c* Elaboro tutti i fogli del terminal di arrivo per la linea 2 liv
015200040607     c
015300040607     c* 1) fogli defluenza
015400040607     c                   EXSR      DEFLUenza
015500040607     c
015600040607     c* 2) fogli vari
015700040607     c                   EXSR      FOGLIvari
015800040607     c                   endif
015900040611     c                   endif
016000040611     c*
016100040607     C     KCAE          READE     AZCAE02L
016200040607     c                   enddo
016300040608     c
016400040608     c                   add       1             yy
016500040608     c                   enddo
016600040607     c
016700960205     C*
016800040604     C* SE NON HO TROVATO NESSUNA MANCAZA STAMPO
016900040604     C                   IF        WMANCA=' '
017000040604     C                   EXSR      STAMPA
017100040604     C                   ENDIF
017200040609     c*
017300040609     c                   clear                   fnlv53ds
017400040609     C                   movel     'C'           D53TLA
017500040609     C                   CALL      'FNLV53R'
017600040609     C                   PARM                    fnlv53ds
017700960202     C*
017800000000     C                   SETON                                        LR
017900941216     C**************************************************************************
018000090309     C*    Lettura fogli defluenza del terminal di arrivo
018100941216     C**************************************************************************
018200040607     C     DEFLUENZA     BEGSR
018300040607     C     Kfogli        SETLL     fnfgv02l
018400040607     C     caetfe        READE     fnfgv02l
018500040607     c
018600040607     c                   dow       not %eof(fnfgv02l)
018700040607     c     fgvttr        lookup    tdef                                   30
018800040611     C* SOLO FOGLI DEFLUENZA GIÀ CHIUSI
018900040611     c                   if        *in30 AND FGVFCF<>' '
019000040607     c                   eval      knpg=1
019100040607     c                   z-add     fgvnfv        knfv
019200040607     c                   eval      kfgs=fgvlnp
019300040608     c                   eval      klna=welalna
019400040607     c
019500040607     c* Verifico se sparati colli per il 2 livello da controllare
019600040607     c                   EXSR      LETBRV
019700040607     C                   ENDIF
019800040607     C     caetfe        READE     fnfgv02l
019900040607     C                   ENDDO
020000040607     c                   endsr
020100040607     C**************************************************************************
020200090309     C*    lettura fogli vari      del terminal di arrivo
020300040607     C**************************************************************************
020400040609     C     FOGLIvari     BEGSR
020500040607     C     Kfogli        SETLL     fnfvv07l
020600040607     C     caetfe        READE     fnfvv07l
020700040607     c
020800040607     c                   dow       not %eof(fnfvv07l)
020900040607     c                   eval      knpg=fvvnpg
021000040607     c                   z-add     fvvnfv        knfv
021100040607     c                   eval      kfgs=fvvfgs
021200040608     c                   eval      klna=welalna
021300040607     c
021400040607     c* Verifico se sparati colli per il 2 livello da controllare
021500040607     c                   EXSR      LETBRV
021600040609     C
021700040607     C     caetfe        READE     fnfvv07l
021800040607     C                   ENDDO
021900040607     c                   endsr
022000040607     c
022100040607     c**************************************************************************
022200040607     C*    LETTURA SPUNTE DEL FOGLIO TROVATO DEL TERMNALL DI ARRIVO
022300040607     C**************************************************************************
022400040607     c     LETBRV        begsr
022500070117     c     kbrv2         setll     fnbrv02l
022600070117     c     kbrv2         reade     fnbrv02l
022700070117    1c                   dow       not %eof(FnBRV02l)
022800040608     c
022900040608     c* elaboro solo spedizioni con colli di valore
023000090309     c     kbrv7         chain     fiart27l
023100090309    2c                   if        %found(fiart27l)
023200040608     c     kar5          chain     fiar501l
023300040608    3c                   if        %found(fiar501l)
023400040608     c                   movel     ar5uni        dar5gen
023500040608     c                   else
023600040608     c                   clear                   dar5gen
023700040608    3c                   endif
023800040608     c* Verifico se sparati colli per il 2 livello da controllare
023900040609    3c                   if        §ar5bva='V'
024000040608     c                   exsr      ctrarr
024100040608    3c                   endif
024200040608    2c                   endif
024300040608     c
024400070117     c     kbrv2         reade     fnbrv02l
024500040607     c                   enddo
024600040607     c                   endsr
024700040528     C**************************************************************************
024800040608     C*    vedo se collo spuntato al 2 liv in arrivo
024900040528     C**************************************************************************
025000040528     C     CTRARR        BEGSR
025100040609     C                   CLEAR                   Wimm
025200040609     C                   CLEAR                   WSAVB
025300040609     C                   CLEAR                   WDFV
025400040609     C                   CLEAR                   WTROV             1
025500040609     C                   CLEAR                   WSPG
025600040609     c                   clear                   wdataspu
025700040611     c                   setoff                                       13
025800040608     c*
025900070117     c     kbrv7         setll     fnbrv07l
026000070117     c     kbrv7         reade     fnbrv07l
026100070117    1c                   dow       not %eof(fnbrv07l) and wdataspu<p02invdam
026200040608     c* Prendo data foglio
026300040608     c                   clear                   fnlv53ds
026400040608     C                   Z-ADD     brvfgs        D53FGS
026500040608     C                   Z-ADD     brvnfv        D53NFV
026600040608     C                   z-add     brvfle        D53flf
026700040608     C                   MOVEL     brvnpg        D53NPG
026800040608     C                   CALL      'FNLV53R'
026900040608     C                   PARM                    fnlv53ds
027000040609     c* Uso data caricamento spunta se c'e' errore
027100040609    2c                   if        d53err<>*blanks
027200040609     c                   eval      d53dfv=brvdcs
027300040609    2c                   endif
027400040609     c
027500040609     c* SPUNTA DEL P.O.
027600040609    2c                   if        brvfgs=p02fgs
027700040609     c                   eval      wdataspu=d53dfv
027800040609     c                   else
027900040609     C* SPUNTA NON DEL P.O.
028000040609     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
028100040609     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
028200040609     c*  immissione + alta
028300040609     c                   EXSr      RICSPU
028400040609    2c                   endif
028500040608     c
028600040608     c* se trovata spunta con data < leggo altra spunta
028700040609    2c                   if        wdataspu<p02invdam
028800070117     c     kbrv7         reade     fnbrv07l
028900040609    2c                   endif
029000040608     c
029100040608    1c                   enddo
029200040608     c
029300040609     c* SPUNTA DEL P.O. NON TROVATA
029400040609    1c                   if        wdataspu<p02invdam
029500040609     C* TROVATA SPUNTA DI ALTRO P.O. (potrebbe essere il terminal di arrivo)
029600040609     C     WTROV         IFEQ      'S'
029700040609     C                   MOVEL     WSAVB         RECBRV
029800040611     C                   ENDIF
029900040608     C
030000040611     C* SE SPUNTA TROVATA CON DATA >= STAMPO MA NON CREO ANOMALIA
030100040617    2C                   IF        WDFV>=p02invdam  and brvNPG<>2     OR
030200040617    2C                             WDFV>=p02invdam  and brvfgs<>caetfe
030300040608     C                   EVAL      *IN13=*ON
030400040608     C* GIRO LA DATA
030500040608     C                   MOVE      WDFV          G02INV
030600040608     C                   MOVE      *ZEROS        G02DAT
030700040608     C                   MOVEL     3             G02ERR
030800040608     C                   CALL      'XSRDA8'
030900040608     C                   PARM                    WLBDAT
031000040608     C                   MOVEL     G02DAT        ST2DFV            4 0
031100040608     C**
031200040608     C* DECODIFICO LA FILIALE GESTIONE
031300040608     C     BRVFGS        CHAIN     AZORG01L                           32
031400040608    2C  N32ORGDE5        IFNE      *BLANKS
031500040608     C                   MOVEL     ORGDE5        ST2FGS            8
031600040608     C                   ELSE
031700040608     C                   MOVEL     ORGDES        ST2FGS            8
031800040608    2C                   ENDIF
031900040608     C**
032000040608     C* DECODIFICO LA CATEGORIA SPUNTA
032100040608    2C     BRVNPG        IFEQ      3
032200040608     C                   MOVEL     'IMA'         ST2CAT            3
032300040608     C                   MOVE      WSPG          ST2CAT
032400040608   X2C                   ELSE
032500040608     C                   MOVEL     '7N'          kCOD
032600040608     C                   MOVEL(P)  BRVNPG        kKEY
032700040608     C     KTAB2         CHAIN     TABEL                              32
032800040608     C  N32              MOVEL     TBLUNI        ST2CAT
032900040608     C   32              CLEAR                   ST2CAT
033000040608    2C                   ENDIF
033100040608     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
033200040608    2C                   IF        BRVNPG=4
033300040609     C                   Z-ADD     BRVNFV        WNFV5             5 0
033400040610     C     KFVV          CHAIN     FNFVV01l                           32
033500040617RM*  C  N32              MOVEL     FVVDSF        STADSF           17
033600040608    2C                   ENDIF
033700040608    1C                   ENDIF
033800040608     c*
033900040603     c
034000040604     C* MEMORIZZO  DI AVER TROVATO ALMENO UNA MANCANZA
034100040604     C                   EVAL      WMANCA='S'
034200040610     c
034300040610   2AC     *IN13         IFEQ      *OFF
034400040610     c                   exsr      crtanm
034500040610     c                   endif
034600040610     c
034700040604     c                   exsr      stampa
034800040608    1c                   endif
034900040604     c                   endsr
035000040608     C**************************************************************************
035100040608     C* cerco spunta del collo in qualsiasi categoria
035200040608     C**************************************************************************
035300040608     C     RICSPU        BEGSR
035400040608RM*  C*
035500040609     c* Per la stampa considero solo se nonn trovato errore nella ricerca
035600040611     c*  del foglio
035700040609     c                   if        d53err=*blanks
035800070131     c                   movel     brvdfs        brvimm           12 0
035900070131     c                   movel     brvhms        com04             4 0
036000070131     c                   move      com04         brvimm
036100040608    3c                   select
036200040608     c* Se maggiore memorizzo
036300040608     c                   when      d53dfv>wdfv
036400040608     C                   MOVEL     RECBRV        WSAVB
036500070117     C                   MOVEL     brvimm        Wimm
036600040608     C                   MOVEL     d53DFV        WDFV
036700040608     C                   MOVEL     d53spg        WSPG
036800040608     C                   MOVEL     'S'           WTROV             1
036900040608     c
037000040608     c* Se uguale tengo la data ora immissione + alta
037100040608     c                   when      d53dfv=wdfv  and brvimm>wimm
037200070117     C                   MOVEL     brvimm        Wimm
037300040608     C                   MOVEL     RECBRV        WSAVB
037400040608     C                   MOVEL     d53DFV        WDFV
037500040608     C                   MOVEL     d53spg        WSPG
037600040608     C                   MOVEL     'S'           WTROV             1
037700040608     C                   eval      wimm=brvimm
037800040608     c* Se stessa data e ora immissione, tengo cat 3 o 4
037900040611     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
038000070117     C                   MOVEL     brvimm        Wimm
038100040608     C                   MOVEL     RECBRV        WSAVB
038200040608     C                   MOVEL     d53DFV        WDFV
038300040608     C                   MOVEL     d53spg        WSPG
038400040608     C                   MOVEL     'S'           WTROV             1
038500040608     C                   eval      wimm=brvimm
038600040608    3c                   endsl
038700040609     c                   endif
038800040608     c
038900040608     c                   endsr
039000040604     C**************************************************************************
039100040604     C* STAMPO mNCANZA COLLO DI VALORE
039200040604     C**************************************************************************
039300040604     C     STAMPA        BEGSR
039400040604     C*
039500040604     C* STAMPA TESTATA
039600040604    1C   OF              DO
039700040604     C* PRIMA PARTE TESTATA
039800040604     C                   EXCEPT    TES
039900040604     C                   EXCEPT    TES2
040000040604     C                   SETOFF                                       OF
040100040604    1C                   END
040200040604     C* Stampo collo mancante
040300040604     C                   IF        WMANCA='S'
040400040604     C* CHAIN SPEDIZIONE
040500040604     C     KBLP          CHAIN     FNBLP01L
040600040604     C                   IF        %FOUND(FNBLP01L)
040700040604     C* SE C'E' BOLLA E A CAMBIO SPEDIZIONE STAMPO I DATI DELLA SPED
040800040604     C     COMSPE        IFNE      BLPSPE
040900040604     C                   SETON                                        05
041000040604     C                   MOVEL     BLPSPE        COMSPE
041100040604     C                   ENDIF
041200040604     C                   ELSE
041300040604     C                   SETOFF                                       05
041400040604     C                   ENDIF
041500040610     C*
041600040610     C* CHAIN ANOMALIA PER PRENDERE LA DATA E ORA DI CREAZIONE ANOM
041700040610     c                   clear                   stadan
041800040610     c                   clear                   staoan
041900040610     c
042000040611     c                   if        not *in13
042100040611     C                   MOVEL     ANMFLO        w0060             6 0          AAAA/MM/GG
042200040611     C                   z-add     w0060         G02INV                         AAAA/MM/GG
042300040610     C                   MOVE      *ZEROS        G02DAT
042400040610     C                   MOVEL     3             G02ERR
042500040610     C                   CALL      'XSRDA8'
042600040610     C                   PARM                    WLBDAT
042700040611     C                   MOVEL     G02DAT        STADAN            8 0          GG/MM
042800040610     C*
042900040610     C                   MOVE      ANMFLO        STAOAN            4 0
043000040610     C                   endif
043100040604     C*
043200040604     C* DATA SPEDIZIONE
043300040604     C   05              DO
043400040604     C                   Z-ADD     BLPAAS        COMAAS                         ANNO (4 CIFRE)
043500040604     C                   MOVE      BLPDSP        G02INV                         AAAA/MM/GG
043600040604     C                   MOVE      *ZEROS        G02DAT
043700040604     C                   MOVEL     3             G02ERR
043800040604     C                   CALL      'XSRDA8'
043900040604     C                   PARM                    WLBDAT
044000040604     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
044100040604     C*
044200040604     C* SEGNACOLLI NON CONSECUTIVI
044300040604     C     BLPFNS        COMP      'S'                                    04
044400040604     C* SERIE>< 0
044500040604     C     BLPNRS        COMP      0                                  0303
044600040604     C                   END
044700040604     C* MITTENTE E DESTINATARIO DI 17 E 20
044800040604     C                   MOVEL     BLPRSM        STARSM           17
044900040604     C                   MOVEL     BLPRSD        STARSD           20
045000040604     C*
045100040604     C* STAMPO DETTAGLIO
045200040604     C                   EXCEPT    DET
045300040604     C                   SETOFF                                       05
045400040604     c                   else
045500040604     c                   except    OK
045600040604     c                   endif
045700040604     c                   endsr
045800040609     C**************************************************************************
045900040609     C* creo anomalia di mancanza
046000040609     C**************************************************************************
046100040609     C     CRTANM        BEGSR
046200040610     c                   eval      wcrea='S'
046300040610     C*
046400040610     C* CHAIN ANOMALIA PER PRENDERE LA DATA E ORA DI CREAZIONE ANOM
046500040610     C     KANM          CHAIN     FNANM02L
046600040610     c
046700040610     C                   IF        %FOUND(FNANM02L)
046800040610     C* SE APERTA O CHIUSA CON PRATICA IDD, NON RICREO
046900040610     C                   IF        ANMDCH=0 OR ANMCCH='PR'
047000040610     C                   EVAL      WCREA='N'
047100040610     c                   endif
047200040610     c                   endif
047300040610     c
047400040610    3C                   if        wcrea='S'
047500040609     c                   clear                   fnanm000
047600090309     C                   MOVEL     artFLS        ANMFLS
047700090309     C                   MOVEL     artLNA        ANMLNA
047800090309     C                   MOVEL     artNRS        ANMNRS
047900090309     C                   MOVEL     artNSC        ANMSCN
048000090309     C                   Z-ADD     artAAS        ANMAAS
048100090309     C                   MOVEL     artNSP        ANMNSP
048200090309     C                   MOVEL     artLNP        ANMLNP
048201090309     c                   z-add     artnap        anmnps
048300040609     C                   z-add     comcaa        ANMcaa
048400040609     C                   MOVEL     PA5FAS        ANMFSA
048500040609     C                   Z-ADD     p02invdam     ANMDAO
048600040609     C                   MOVEL     p02FGS        ANMFLE
048700040609     C                   MOVEL     §7CTAN        ANMTAN
048800040609     C                   MOVEL     §7CSAN        ANMSAN
048900040609     C                   MOVEL     §7CAIE        ANMAIE
049000040609     C                   Z-ADD     §7CLID        ANMLID
049100040609     C                   MOVE      999           ANMFL4
049200040609     C* CHIUSURA AUTOMATICA
049300040609     C     §7CCHA        IFEQ      'S'
049400040609     C                   Z-ADD     FVVDFV        ANMDCH
049500040609     C                   MOVEL     PA5FAS        ANMFSC
049600040609     C                   END
050300040609     c
050400040609     c* imposto data e ora di creazione
051100050707     C                   TIME                    Wtime            14 0
051200050707     c                   movel     wtime         wora4             4 0
051300050707     c                   move      wora4         anmflo
051400050707     c
051500050707     C                   CLEAR                   WLBDAt
051600050707     C                   MOVE      Wtime         G02DAT
051700050707     C                   CALL      'XSRDA8'
051800050707     C                   PARM                    WLBDAt
051900050707     C                   MOVE      G02INV        wdata             6 0
052000050707     C                   MOVEl     wdata         anmflo
052100040609     c
052200040609     c* imposto se flag di valore
052300040609     c                   movel     §ar5bva       anmft4
052400040609     c
052500090309     C                   MOVEL     artLNP        ANMFL1
052600040609     C*
052700040610     c                   if        not %found(fnanm02l)
052800040609     C                   WRITE     FNANM000
052900040610     c                   else
053000040610     c                   update    fnanm000
053100040611     c                   endif
053200040611     c
053300040610     c                   else
053400040610     c
053500040610     c* Se non devo creare e chainata, disalloco
053600040610     c                   unlock    fnanm02l
053700040610    3C                   ENDif
053800040609     c                   endsr
053900960202     C*
054000951115     C*--- SR INIZIALE -----------------------------------------------*
054100951115     C     *INZSR        BEGSR
054200040609     c
054300040609     C     *ENTRY        PLIST
054400040609     C                   PARM                    KPJBA
054500040609     c                   movel     kpjbu         para02
054600040609     C*
054700040609     C                   Z-ADD     1             CODUT
054800040609     C                   CALL      'X§PARUT'
054900040609     C                   PARM                    UT§DSE0F
055000040609     C                   MOVEL     REC80         CNCR80
055100040609     C                   MOVEL     RAGUT         RSUT             20
055200040607     c
055300040607     c* Carico i tipi traino defluenza
055400040607     c                   clear                   x
055500040607     C     KTAB          setll     TABEL
055600040607     C     KTAB          reade     TABEL
055700040607     c                   dow       not %eof(tabel00f)
055800040607     c                   if        tblflg=' '
055900040607     c                   movel     tbluni        dstv
056000040609     c                   if        §tvdef='S'
056100040607     c                   add       1             X                 3 0
056200040607     c                   movel     tblcod        tdef(x)
056300040607     c                   endif
056400040607     c                   endif
056500040607     C     KTAB          reade     TABEL
056600040607     c                   enddo
056700040609     c
056800040609     c
056900040609     c                   time                    w0140            14 0
057000040609     c                   move      w0140         udate8            8 0
057100040609     c                   movel     w0140         utime             6 0
057200040609     C* PRENDO FASE ANOMALIA
057300040609     C                   CLEAR                   PARAM5
057400040609     C                   MOVEL     'L'           PA5TLA
057500040609     C                   MOVEL     2             PA5NPG
057600040609     C                   CALL      'TRUL11R'
057700040609     C                   PARM                    PARAM5
057800040609     C*
057900040609     C                   MOVEL     '7C'          kCOD
058000040609     C                   MOVEL(P)  comCAA        kKEY
058100040609     C     KTAB2         CHAIN     TABEL00F                           35
058200040609     C  N35              MOVEL     TBLUNI        DS7C
058300040609     C   35              MOVEL     *BLANKS       DS7C
058400951115     C*
058500951115     C****  KLIST  ****
058600040607     C     KTAB          KLIST
058700040607     C                   KFLD                    codut
058800040607     C                   KFLD                    kcod
058900040608     C     KTAB2         KLIST
059000040608     C                   KFLD                    codut
059100040608     C                   KFLD                    kcod
059200040608     C                   KFLD                    kkey
059300040607     C     KCAE          KLIST
059400040607     C                   KFLD                    KEPA
059500040608     C                   KFLD                    Welalna
059600040607     C     Kfogli        KLIST
059700040607     C                   KFLD                    caetfe
059800040607     C                   KFLD                    p02invdam
059900040607     C     Kbrv2         KLIST
060000040607     C                   KFLD                    knpg
060100040607     C                   KFLD                    knfv
060200040607     C                   KFLD                    kfgs
060300040607     C                   KFLD                    klna
060400951115     C*
060500040608     C     Kbrv7         KLIST
060600040608     C                   KFLD                    ter_brvlnp
060700040608     C                   KFLD                    TER_brvlna
060800040608     C                   KFLD                    TER_brvnrs
060900040608     C                   KFLD                    TER_brvnsc
061000040608     C     KFVV          KLIST
061100040608     C                   KFLD                    BRVNPG
061200040608     C                   KFLD                    WNFV5
061300040608     C                   KFLD                    BRVFGS
061400040608     C     Kar5          KLIST                                                  *
061500090309     C                   KFLD                    artAAS                         *
061600090309     C                   KFLD                    artLNP                         *
061700090309     C                   KFLD                    artNRS                         *
061800090309     C                   KFLD                    artNSP                         *
061900040608     C                   KFLD                    ktrd                           *
062000040608     C     Kblp          KLIST                                                  *
062100090309     C                   KFLD                    artAAS                         *
062200090309     C                   KFLD                    artLNP                         *
062300090309     C                   KFLD                    artNRS                         *
062400090309     C                   KFLD                    artNSP                         *
062500040609     C     KANM          KLIST                                                  FNANM02L
062600090309     C                   KFLD                    artFLS                         *
062700090309     C                   KFLD                    arTLNA                         *
062800090309     C                   KFLD                    arTNRS                         *
062900090309     C                   KFLD                    arTNSC                         *
063000040609     C                   KFLD                    comCAA                         *
063100951115     C                   ENDSR
063200040604     C**************************************************************************
063300040608     OPRTF198   E            TES              02
063400040604     O                       STA(1)              66
063500040604     O                       STA(2)             132
063600040604     O                       STA(13)            198
063700040604     O                       RSUT                20
063800040604     O                       UDATE8             181 '  /  /    '
063900040604     O                       PAGE          Z    198
064000040604     O          E            TES         1
064100040604     O                       UTIME              179 '  :  :  '
064200040604     O          E            TES         2
064300040604     O                       STA(3)              66
064400040609RM*  O                       p02FGS              20
064500040608     O                       DESFGS              41
064600040608     O                       p02dam        8     76
064700040604     O          E            TES2        2
064800040604     O                       STA(5)              66
064900040604     O                       STA(8)             132
065000040610     O                       STA(14)            198
065100040604     O          E            TES2        1  1
065200040604     O                       STA(6)              66
065300040604     O                       STA(9)             132
065400040610     O                       STA(15)            198
065500040604     O*
065600040604     O          E            DET         1
065700040604     O                  05   BLPAAS        Z
065800040604     O                  05   BLPLNP        Z   +  1
065900040604     O                  05 03BLPNRS        Z   +  1
066000040604     O                  05 03                  +  0 '-'
066100040604     O                  05   BLPNSP        Z   +  0
066200040604     O                  05   STADSP            +  1 '  /  '
066300040604     O                  05   BLPLNA        Z   +  1
066400040604     O                  05   STARSM            +  1
066500040604     O                  05   STARSD            +  1
066600040604     O                  05N04BLPNCD        Z   +  1
066700040604     O                  05N04                  +  0 '/'
066800040604     O                  05N04BLPNCA        Z   +  0
066900040604     O                  05 04                    84 'NON SEQUENZIALI'
067000040604     O                  05   BLPZNC            +  1
067100040604     O                       BRVLNP            +  4
067200040604     O                       BRVLNA            +  2
067300040604     O                       BRVNRS        Z   +  3
067400040604     O                       BRVNSC        Z   +  4
067500040617     O                       STADAN            +  5 '  /  /    '
067600040610     O                       STAOAN            +  2 '  :  '
067700040617     O                     13brvFGS             142
067800040617     O                     13                   143 '-'
067900040617     O                     13ST2FGS             151
068000070117     O                     13ST2CAT             155
068100070117     O                     13brvNPG             157
068200070117     O                     13                   158 '-'
068300070117     O                     13brvNFV        Z    164
068400070117     O                     13ST2DFV             171 '  /  '
068500070117     O                     13brvhms             180 '  :  :  '
068600040610     O                     13STADSF             198
068700040604     O          E            OK          3
068800040604     O                       STA(11)             66
068900040604     O          E            OK          0  0
069000040604     O                       STA(11)             66
069100040604     O**************************************************************************
069200040604** STA
069300040617XXXXXXXXXXXXXXXXXXXX                                                 1
069400040607 ** COLLI DI VALORE MANCANTI AL 2 LIVELLO IN ARRIVO **               2
069500040608PUNTO OPERATIVO: xxx-XXXXXXXXXXXXXXXXXXX      DATA ARRIVO COLLI:     3
069600040607
069700040604    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO         5
069800040604ANNO LNP SER NUMERO  DATA ARR                                        6
069900040604XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx   7
070000040617     SEGNACOLLI    ZON   C O L L O   M A N C A N T E  CREAZIONE AN
070100040617      DAL /   AL   ARR   LNP  LNA  SERIE   NUMERO        DATA
070200040617XX XXXXXXX/XXXXXXX XX    XXX  XXX   XX    XXXXXXX     XX/XX/XXXx
070300040604             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
070400040604 **  FINE  STAMPA  **                                               12
070500040607                         FNLRV2R                          PAG.XXXX  13
070600040617OMALIA   S  P   U   N   T   A   T   O      D  A
070700070117 ORA   P.Operativo    Foglio       del   Ore
070800070117xx:xx  XXX-XXXXXXXX ARR 2-XXXXXX  XX/XX xx:xx:xx  xxxxxxxxxxxxxxxxx
070900040604**
071000040604OVRPRTF FILE(PRTF198) USRDTA('ColValore')
