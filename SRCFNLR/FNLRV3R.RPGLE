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
001800040608     Ffnblt27l  IF   E           K DISK
001900040609     Ffnart27l  IF   E           K DISK
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
003100160920     D* TIPO zone consegna
003200160920     D  P02Tpm1               20     21
003300160920     D  p02Tpm2               22     23
003400040609     D*** PARAMETRI PER RICERCA FASE ANOMALIA    - TRUL11R -
003500040609     D PARAM5          DS
003600040609     D  PA5TLA                 1      1
003700040609     D  PA5NPG                 2      2
003800040609     D  PA5SPG                 3      3
003900040609     D  PA5FAS                 4      4
004000951115     D*
004100911010     D WLBDAT          DS
004200941221     D  G02DAT                 1      8  0
004300941221     D  G02INV                 9     16  0
004400941221     D  G02ERR                17     17
004500941221     D  G02TGI                18     22  0
004600960304     D WGIDAT          DS                  INZ
004700960304     D  GIODAT                 1      8  0
004800960304     D  GIOINV                 9     16  0
004900960304     D  GIOTGI                17     21  0
005000040604     D                 DS
005100040604     D  BLPAAS                 1      4  0
005200040604     D  BLPLNP                 5      7  0
005300040604     D  BLPNRS                 8      9  0
005400040604     D  BLPNSP                10     16  0
005500040604     D  BLPSPE                 1     16  0
005600040604     D                 DS
005700040604     D  COMAAS                 1      4  0
005800040604     D  BLPMGS                 5      8  0
005900040604     D  BLPDSP                 1      8  0
006000040604     D
006100040610     D STA             S             66    DIM(16) CTDATA PERRCD(1)             SKIERE STAMPA
006200040604     D CMD             S              1    DIM(80) CTDATA PERRCD(80)            COMANDO QCMDEXC
006300040609     D TDEF            S              1    DIM(20)                              FV GIA' SELEZ
006400160920     D zone            S              2    DIM(200)                             COMANDO QCMDEXC
006500160920     D zone12          S              2    DIM(200)                             COMANDO QCMDEXC
006600960304     D*
006700040604     D CNCR80        E DS
006800160920     D ds03          E DS
006900040604     D og143         E DS
007000040604     D KPJBA         E DS
007100030722     D UT§DSE0F      E DS
007200040608     D FNLV53DS      E DS
007300040607     D DSTV          E DS
007400040609     D DS7c          E DS
007500040608     D DAR5GEN       E DS
007600070117     D RECBRV        E DS                  EXTNAME(FNBRV00F)
007700070117     D WSAVB         E DS                  EXTNAME(FNBRV00F)
007800040608     d                                     prefix(S_)
007900040608     D*
008000040608     D* DS PER TRUL06R - CARICAMENTO £X
008100040608     D DSUL06        E DS                  EXTNAME(TRUL06DS)
008200040608     D  LIN                    1     90  0
008300040608     D                                     DIM(30)                              FIL. COMODO
008400040608     D L6              S              3  0 DIM(30)                              F. MEC    GES
008500040607     d KEPA            s                   Like(CAEEPA) INZ('A')
008600040607     d KCOD            s                   Like(tblcod) INZ('TV')
008700040608     d Kkey            s                   Like(tblkey)
008800040607     d Knpg            s                   Like(brvnpg)
008900040607     d Knfv            s                   Like(brvnfv)
009000040607     d Kfgs            s                   Like(brvfgs)
009100040607     d Klna            s                   Like(brvlna)
009200040609     d wdataspu        s                   Like(d53dfv)
009300040608     d wdfv            S                   LIKE(d53dfv)
009400040608     d wimm            S                   LIKE(brvimm)
009500040608     d WSPG            S                   LIKE(FVVSPG)
009600040608     d ktrd            S                   LIKE(ar5trd) inz('GEN')
009700040609     d comcaa          S                   LIKE(anmcaa) inz(015)
009800040609     d comspe          s                   Like(blpspe)
009900040609     d welalna         s                   Like(bltlna)
010000040609     d Wmanca          s              1
010100040610     d WCREa           s              1
010200160920     d indMP           S              4  0
010300160920     d ind12           S              4  0
010400160920     d statipo         S             21
010500160920     d statipo2        S             21
010600160920     D CPARCEL         C                   CONST('     P A R C E L     ')
010700160920     D CMessag         C                   CONST('M E S S A G G E R I E')
010800160920     D Cparc1          C                   CONST('PARCEL: 1° turno     ')
010900160920     D Cparc2          C                   CONST('PARCEL: 2° turno     ')
011000000000     C*---------------------------------------------------------------*
011100911010     C* INDICATORI
011200911010     C*---------------------------------------------------------------*
011300040608     c*  04   -
011400040608     c*  05   -
011500040608     c*  13   - trovata spunta di altro p.o.
011600911010     C*---------------------------------------------------------------*
011700040607     c* Apro file di stampa
011800040607     C                   Z-ADD     80            LUNG             15 5
011900040607     C                   MOVEL     *BLANKS       COMMAN           80
012000040607     C                   MOVEA     CMD           COMMAN
012100040607     C                   CALL      'QCMDEXC'                            H1      ERRORE=USCITA
012200040607     C                   PARM                    COMMAN
012300040607     C                   PARM                    LUNG
012400040607     C*
012500040607RM*  C                   OPEN      PRTF198
012600040607     C                   SETON                                        OF
012700040608     C*
012800040608     c* carico £6 del p.o. gestione
012900040608     C                   CLEAR                   DSUL06
013000040608     C                   MOVE      '£6'          D06COD
013100040608     C                   MOVEL     p02FGS        D06KEY
013200040608     C                   MOVEL     'L'           D06TLA
013300040608     C                   MOVEL     DSUL06        KPJBU
013400040608     C*
013500040608     C                   CALL      'TRUL06R'
013600040608     C                   PARM                    KPJBA
013700040608     C                   MOVEL     KPJBU         DSUL06
013800040608     C                   MOVEA     LIN           L6
013900040608     c*
014000040608     c* Decodifico p.o. richiesto
014100040608     c     p02fgs        chain     azorg01l
014200040608     c                   if        %found(azorg01l)
014300040608     c                   movel     orgdes        desfgs
014400040608     c                   else
014500040608     c                   clear                   desfgs           20
014600040608     c                   endif
014700040608     c
014800040608     c* elaboro tutti i p.o. della £6
014900040608     c                   z-add     1             yy                3 0
015000040608     c                   dow       l6(yy)>0
015100040608     c                   eval      Welalna=l6(yy)
015200040607     C*
015300040607     c* Lettura di azcae per prendere i terminal di arrivo
015400040607     C     KCAE          SETLL     AZCAE02L
015500040607     C     KCAE          READE     AZCAE02L
015600040607     c
015700040607     C                   DOW       NOT %EOF(azcae02l)
015800040607     c* controllo decorrenza/scadenza
015900040607     c                   if        caeatb=' '  and
016000040607     c                             caedde<=p02invdam and caedsc>=p02invdam
016100040611     c*
016200040611     c* Escludo  se stesso come terminal non ha senso controllare
016300040611     c                   if        caetfe<>p02fgs
016400040607     c
016500040607     c* Elaboro tutti i fogli del terminal di arrivo per la linea 2 liv
016600040607     c
016700040607     c* 1) fogli defluenza
016800040607     c                   EXSR      DEFLUenza
016900040607     c
017000040607     c* 2) fogli vari
017100040607     c                   EXSR      FOGLIvari
017200040607     c                   endif
017300040611     c                   endif
017400040611     c*
017500040607     C     KCAE          READE     AZCAE02L
017600040607     c                   enddo
017700040608     c
017800040608     c                   add       1             yy
017900040608     c                   enddo
018000040607     c
018100960205     C*
018200040604     C* SE NON HO TROVATO NESSUNA MANCAZA STAMPO
018300040604     C                   IF        WMANCA=' '
018400040604     C                   EXSR      STAMPA
018500040604     C                   ENDIF
018600040609     c*
018700040609     c                   clear                   fnlv53ds
018800040609     C                   movel     'C'           D53TLA
018900040609     C                   CALL      'FNLV53R'
019000040609     C                   PARM                    fnlv53ds
019100960202     C*
019200000000     C                   SETON                                        LR
019300941216     C**************************************************************************
019400040607     C*    Lettura fogli defluenza del temrinal di arrivo
019500941216     C**************************************************************************
019600040607     C     DEFLUENZA     BEGSR
019700040607     C     Kfogli        SETLL     fnfgv02l
019800040607     C     caetfe        READE     fnfgv02l
019900040607     c
020000040607     c                   dow       not %eof(fnfgv02l)
020100040607     c     fgvttr        lookup    tdef                                   30
020200040611     C* SOLO FOGLI DEFLUENZA GIÀ CHIUSI
020300040611     c                   if        *in30 AND FGVFCF<>' '
020400040607     c                   eval      knpg=1
020500040607     c                   z-add     fgvnfv        knfv
020600040607     c                   eval      kfgs=fgvlnp
020700040608     c                   eval      klna=welalna
020800040607     c
020900040607     c* Verifico se sparati colli per il 2 livello da controllare
021000040607     c                   EXSR      LETBRV
021100040607     C                   ENDIF
021200040607     C     caetfe        READE     fnfgv02l
021300040607     C                   ENDDO
021400040607     c                   endsr
021500040607     C**************************************************************************
021600040607     C*    lettura fogli vari      del temrinal di arrivo
021700040607     C**************************************************************************
021800040609     C     FOGLIvari     BEGSR
021900040607     C     Kfogli        SETLL     fnfvv07l
022000040607     C     caetfe        READE     fnfvv07l
022100040607     c
022200040607     c                   dow       not %eof(fnfvv07l)
022300040607     c                   eval      knpg=fvvnpg
022400040607     c                   z-add     fvvnfv        knfv
022500040607     c                   eval      kfgs=fvvfgs
022600040608     c                   eval      klna=welalna
022700040607     c
022800040607     c* Verifico se sparati colli per il 2 livello da controllare
022900040607     c                   EXSR      LETBRV
023000040609     C
023100040607     C     caetfe        READE     fnfvv07l
023200040607     C                   ENDDO
023300040607     c                   endsr
023400040607     c
023500040607     c**************************************************************************
023600040607     C*    LETTURA SPUNTE DEL FOGLIO TROVATO DEL TERMNALL DI ARRIVO
023700040607     C**************************************************************************
023800040607     c     LETBRV        begsr
023900070117     c     kbrv2         setll     fnbrv02l
024000070117     c     kbrv2         reade     fnbrv02l
024100070117    1c                   dow       not %eof(FnBRV02l)
024200040608     c
024300040608     c* elaboro solo spedizioni con colli di valore
024400040608     c     kbrv7         chain     fnblt27l
024500040608    2c                   if        %found(fnblt27l)
024600040608     c     kar5          chain     fiar501l
024700040608    3c                   if        %found(fiar501l)
024800040608     c                   movel     ar5uni        dar5gen
024900040608     c                   else
025000040608     c                   clear                   dar5gen
025100040608    3c                   endif
025200040608     c* Verifico se sparati colli per il 2 livello da controllare
025300040609    3c                   if        §ar5bva='V'
025400160920     c
025500160920     c* SE parzializzazione  per zona, controllo da fnblp
025600160920    4c                   if        wselzona='S'
025700160920     C     KBLP          CHAIN     FNBLP01L
025800160920     c                   if        not %found(fnblp01l)
025900160920     c                   clear                   blpznc
026000160920     c                   endif
026100160920     c
026200160920     c                   movel     blpznc        alfzona           2
026300160920
026400160920    5c                   if        wparc='S' or wmess='S'
026500160920     c                   eval      indMP =%lookup(alfzona:zone)
026600160920    5c                   endif
026700160920    5c                   if        wparc1='S' or wparc2='S'
026800160920     c                   eval      ind12 =%lookup(alfzona:zone12)
026900160920    5c                   endif
027000160920
027100160920    4c                   endif
027200160920     c
027300160920     c
027400160920    4c                   if        wselzona=' ' or  indmp>0 or  ind12>0
027500040608     c                   exsr      ctrarr
027600160920    4c                   endif
027700160920    3c                   endif
027800040608    2c                   endif
027900040608     c
028000070117     c     kbrv2         reade     fnbrv02l
028100040607     c                   enddo
028200040607     c                   endsr
028300040528     C**************************************************************************
028400040608     C*    vedo se collo spuntato al 2 liv in arrivo
028500040528     C**************************************************************************
028600040528     C     CTRARR        BEGSR
028700040609     C                   CLEAR                   Wimm
028800040609     C                   CLEAR                   WSAVB
028900040609     C                   CLEAR                   WDFV
029000040609     C                   CLEAR                   WTROV             1
029100040609     C                   CLEAR                   WSPG
029200040609     c                   clear                   wdataspu
029300040611     c                   setoff                                       13
029400040608     c*
029500070117     c     kbrv7         setll     fnbrv07l
029600070117     c     kbrv7         reade     fnbrv07l
029700070117    1c                   dow       not %eof(fnbrv07l) and wdataspu<p02invdam
029800040608     c* Prendo data foglio
029900040608     c                   clear                   fnlv53ds
030000040608     C                   Z-ADD     brvfgs        D53FGS
030100040608     C                   Z-ADD     brvnfv        D53NFV
030200040608     C                   z-add     brvfle        D53flf
030300040608     C                   MOVEL     brvnpg        D53NPG
030400040608     C                   CALL      'FNLV53R'
030500040608     C                   PARM                    fnlv53ds
030600040609     c* Uso data caricamento spunta se c'e' errore
030700040609    2c                   if        d53err<>*blanks
030800040609     c                   eval      d53dfv=brvdcs
030900040609    2c                   endif
031000040609     c
031100040609     c* SPUNTA DEL P.O.
031200040609    2c                   if        brvfgs=p02fgs
031300040609     c                   eval      wdataspu=d53dfv
031400040609     c                   else
031500040609     C* SPUNTA NON DEL P.O.
031600040609     c* Per la stampa cerco una spunta di qualsiasi categoria, con data
031700040609     c*  foglio + alta, >= alla data foglio e a partità di data con data/or
031800040609     c*  immissione + alta
031900040609     c                   EXSr      RICSPU
032000040609    2c                   endif
032100040608     c
032200040608     c* se trovata spunta con data < leggo altra spunta
032300040609    2c                   if        wdataspu<p02invdam
032400070117     c     kbrv7         reade     fnbrv07l
032500040609    2c                   endif
032600040608     c
032700040608    1c                   enddo
032800040608     c
032900040609     c* SPUNTA DEL P.O. NON TROVATA
033000040609    1c                   if        wdataspu<p02invdam
033100040609     C* TROVATA SPUNTA DI ALTRO P.O. (potrebbe essere il terminal di arrivo)
033200040609     C     WTROV         IFEQ      'S'
033300040609     C                   MOVEL     WSAVB         RECBRV
033400040611     C                   ENDIF
033500040608     C
033600040611     C* SE SPUNTA TROVATA CON DATA >= STAMPO MA NON CREO ANOMALIA
033700040617    2C                   IF        WDFV>=p02invdam  and brvNPG<>2     OR
033800040617    2C                             WDFV>=p02invdam  and brvfgs<>caetfe
033900040608     C                   EVAL      *IN13=*ON
034000040608     C* GIRO LA DATA
034100040608     C                   MOVE      WDFV          G02INV
034200040608     C                   MOVE      *ZEROS        G02DAT
034300040608     C                   MOVEL     3             G02ERR
034400040608     C                   CALL      'XSRDA8'
034500040608     C                   PARM                    WLBDAT
034600040608     C                   MOVEL     G02DAT        ST2DFV            4 0
034700040608     C**
034800040608     C* DECODIFICO LA FILIALE GESTIONE
034900040608     C     BRVFGS        CHAIN     AZORG01L                           32
035000040608    2C  N32ORGDE5        IFNE      *BLANKS
035100040608     C                   MOVEL     ORGDE5        ST2FGS            8
035200040608     C                   ELSE
035300040608     C                   MOVEL     ORGDES        ST2FGS            8
035400040608    2C                   ENDIF
035500040608     C**
035600040608     C* DECODIFICO LA CATEGORIA SPUNTA
035700040608    2C     BRVNPG        IFEQ      3
035800040608     C                   MOVEL     'IMA'         ST2CAT            3
035900040608     C                   MOVE      WSPG          ST2CAT
036000040608   X2C                   ELSE
036100040608     C                   MOVEL     '7N'          kCOD
036200040608     C                   MOVEL(P)  BRVNPG        kKEY
036300040608     C     KTAB2         CHAIN     TABEL                              32
036400040608     C  N32              MOVEL     TBLUNI        ST2CAT
036500040608     C   32              CLEAR                   ST2CAT
036600040608    2C                   ENDIF
036700040608     C* SE CAT 4 AGGANCIO FNFVV PER AVERE DECODIFICA DEL PADRONCINO
036800040608    2C                   IF        BRVNPG=4
036900040609     C                   Z-ADD     BRVNFV        WNFV5             5 0
037000040610     C     KFVV          CHAIN     FNFVV01l                           32
037100040617RM*  C  N32              MOVEL     FVVDSF        STADSF           17
037200040608    2C                   ENDIF
037300040608    1C                   ENDIF
037400040608     c*
037500040603     c
037600040604     C* MEMORIZZO  DI AVER TROVATO ALMENO UNA MANCANZA
037700040604     C                   EVAL      WMANCA='S'
037800040610     c
037900040610   2AC     *IN13         IFEQ      *OFF
038000040610     c                   exsr      crtanm
038100040610     c                   endif
038200040610     c
038300040604     c                   exsr      stampa
038400040608    1c                   endif
038500040604     c                   endsr
038600040608     C**************************************************************************
038700040608     C* cerco spunta del collo in qualsiasi categoria
038800040608     C**************************************************************************
038900040608     C     RICSPU        BEGSR
039000040608RM*  C*
039100040609     c* Per la stampa considero solo se nonn trovato errore nella ricerca
039200040611     c*  del foglio
039300040609     c                   if        d53err=*blanks
039400070131     c                   movel     brvdfs        brvimm           12 0
039500070131     c                   movel     brvhms        com04             4 0
039600070131     c                   move      com04         brvimm
039700040608    3c                   select
039800040608     c* Se maggiore memorizzo
039900040608     c                   when      d53dfv>wdfv
040000040608     C                   MOVEL     RECBRV        WSAVB
040100070117     C                   MOVEL     brvimm        Wimm
040200040608     C                   MOVEL     d53DFV        WDFV
040300040608     C                   MOVEL     d53spg        WSPG
040400040608     C                   MOVEL     'S'           WTROV             1
040500040608     c
040600040608     c* Se uguale tengo la data ora immissione + alta
040700040608     c                   when      d53dfv=wdfv  and brvimm>wimm
040800070117     C                   MOVEL     brvimm        Wimm
040900040608     C                   MOVEL     RECBRV        WSAVB
041000040608     C                   MOVEL     d53DFV        WDFV
041100040608     C                   MOVEL     d53spg        WSPG
041200040608     C                   MOVEL     'S'           WTROV             1
041300040608     C                   eval      wimm=brvimm
041400040608     c* Se stessa data e ora immissione, tengo cat 3 o 4
041500040611     c                   when      d53dfv=wdfv and brvimm=wimm and brvnpg<>2
041600070117     C                   MOVEL     brvimm        Wimm
041700040608     C                   MOVEL     RECBRV        WSAVB
041800040608     C                   MOVEL     d53DFV        WDFV
041900040608     C                   MOVEL     d53spg        WSPG
042000040608     C                   MOVEL     'S'           WTROV             1
042100040608     C                   eval      wimm=brvimm
042200040608    3c                   endsl
042300040609     c                   endif
042400040608     c
042500040608     c                   endsr
042600040604     C**************************************************************************
042700040604     C* STAMPO mNCANZA COLLO DI VALORE
042800040604     C**************************************************************************
042900040604     C     STAMPA        BEGSR
043000040604     C*
043100040604     C* STAMPA TESTATA
043200040604    1C   OF              DO
043300040604     C* PRIMA PARTE TESTATA
043400040604     C                   EXCEPT    TES
043500040604     C                   EXCEPT    TES2
043600040604     C                   SETOFF                                       OF
043700040604    1C                   END
043800040604     C* Stampo collo mancante
043900040604     C                   IF        WMANCA='S'
044000040604     C* CHAIN SPEDIZIONE
044100040604     C     KBLP          CHAIN     FNBLP01L
044200040604     C                   IF        %FOUND(FNBLP01L)
044300040604     C* SE C'E' BOLLA E A CAMBIO SPEDIZIONE STAMPO I DATI DELLA SPED
044400040604     C     COMSPE        IFNE      BLPSPE
044500040604     C                   SETON                                        05
044600040604     C                   MOVEL     BLPSPE        COMSPE
044700040604     C                   ENDIF
044800040604     C                   ELSE
044900040604     C                   SETOFF                                       05
045000040604     C                   ENDIF
045100040610     C*
045200040610     C* CHAIN ANOMALIA PER PRENDERE LA DATA E ORA DI CREAZIONE ANOM
045300040610     c                   clear                   stadan
045400040610     c                   clear                   staoan
045500040610     c
045600040611     c                   if        not *in13
045700040611     C                   MOVEL     ANMFLO        w0060             6 0          AAAA/MM/GG
045800040611     C                   z-add     w0060         G02INV                         AAAA/MM/GG
045900040610     C                   MOVE      *ZEROS        G02DAT
046000040610     C                   MOVEL     3             G02ERR
046100040610     C                   CALL      'XSRDA8'
046200040610     C                   PARM                    WLBDAT
046300040611     C                   MOVEL     G02DAT        STADAN            8 0          GG/MM
046400040610     C*
046500040610     C                   MOVE      ANMFLO        STAOAN            4 0
046600040610     C                   endif
046700040604     C*
046800040604     C* DATA SPEDIZIONE
046900040604     C   05              DO
047000040604     C                   Z-ADD     BLPAAS        COMAAS                         ANNO (4 CIFRE)
047100040604     C                   MOVE      BLPDSP        G02INV                         AAAA/MM/GG
047200040604     C                   MOVE      *ZEROS        G02DAT
047300040604     C                   MOVEL     3             G02ERR
047400040604     C                   CALL      'XSRDA8'
047500040604     C                   PARM                    WLBDAT
047600040604     C                   MOVEL     G02DAT        STADSP            4 0          GG/MM
047700040604     C*
047800040604     C* SEGNACOLLI NON CONSECUTIVI
047900040604     C     BLPFNS        COMP      'S'                                    04
048000040604     C* SERIE>< 0
048100040604     C     BLPNRS        COMP      0                                  0303
048200040604     C                   END
048300040604     C* MITTENTE E DESTINATARIO DI 17 E 20
048400040604     C                   MOVEL     BLPRSM        STARSM           17
048500040604     C                   MOVEL     BLPRSD        STARSD           20
048600040604     C*
048700040604     C* STAMPO DETTAGLIO
048800040604     C                   EXCEPT    DET
048900040604     C                   SETOFF                                       05
049000040604     c                   else
049100040604     c                   except    OK
049200040604     c                   endif
049300040604     c                   endsr
049400040609     C**************************************************************************
049500040609     C* creo anomalia di mancanza
049600040609     C**************************************************************************
049700040609     C     CRTANM        BEGSR
049800040610     c                   eval      wcrea='S'
049900040610     C*
050000040610     C* CHAIN ANOMALIA PER PRENDERE LA DATA E ORA DI CREAZIONE ANOM
050100040610     C     KANM          CHAIN     FNANM02L
050200040610     c
050300040610     C                   IF        %FOUND(FNANM02L)
050400040610     C* SE APERTA O CHIUSA CON PRATICA IDD, NON RICREO
050500040610     C                   IF        ANMDCH=0 OR ANMCCH='PR'
050600040610     C                   EVAL      WCREA='N'
050700040610     c                   endif
050800040610     c                   endif
050900040610     c
051000040610    3C                   if        wcrea='S'
051100040609     c                   clear                   fnanm000
051200040609     C                   MOVEL     blTFLS        ANMFLS
051300040609     C                   MOVEL     blTLNA        ANMLNA
051400040609     C                   MOVEL     blTNRS        ANMNRS
051500040609     C                   MOVEL     blTNSC        ANMSCN
051600040609     C                   Z-ADD     blTAAS        ANMAAS
051700040609     C                   MOVEL     blTNSP        ANMNSP
051800040609     C                   MOVEL     blTLNP        ANMLNP
051900040609     C                   z-add     comcaa        ANMcaa
052000040609     C                   MOVEL     PA5FAS        ANMFSA
052100040609     C                   Z-ADD     p02invdam     ANMDAO
052200040609     C                   MOVEL     p02FGS        ANMFLE
052300040609     C                   MOVEL     §7CTAN        ANMTAN
052400040609     C                   MOVEL     §7CSAN        ANMSAN
052500040609     C                   MOVEL     §7CAIE        ANMAIE
052600040609     C                   Z-ADD     §7CLID        ANMLID
052700040609     C                   MOVE      999           ANMFL4
052800040609     C* CHIUSURA AUTOMATICA
052900040609     C     §7CCHA        IFEQ      'S'
053000040609     C                   Z-ADD     FVVDFV        ANMDCH
053100040609     C                   MOVEL     PA5FAS        ANMFSC
053200040609     C                   END
053300040609     c* chain su fnart per prendere pistola della data arrivo
053400040609     c     kbrv7         chain     fnart27l
053500040609     c
053600040609     c                   if        %found(fnart27l)
053700040609     c                   z-add     artnap        anmnps
053800040609     c                   endif
053900040609     c
054000040609     c* imposto data e ora di creazione
054100040609     c                   time                    wora              6 0
054200040609     c                   movel     wora          wora4             4 0
054300050707     c**                 z-add     *date         wdata             6 0
054400050707     c**                 movel     wdata         anmflo
054500050707     c**                 move      wora4         anmflo
054600050707     c
054700050707     C                   TIME                    Wtime            14 0
054800050707     c                   movel     wtime         wora4             4 0
054900050707     c                   move      wora4         anmflo
055000050707     c
055100050707     C                   CLEAR                   WLBDAt
055200050707     C                   MOVE      Wtime         G02DAT
055300050707     C                   CALL      'XSRDA8'
055400050707     C                   PARM                    WLBDAt
055500050707     C                   MOVE      G02INV        wdata             6 0
055600050707     C                   MOVEl     wdata         anmflo
055700040609     c
055800040609     c* imposto se flag di valore
055900040609     c                   movel     §ar5bva       anmft4
056000040609     c
056100040609     C                   MOVEL     bltLNP        ANMFL1
056200040609     C*
056300040610     c                   if        not %found(fnanm02l)
056400040609     C                   WRITE     FNANM000
056500040610     c                   else
056600040610     c                   update    fnanm000
056700040611     c                   endif
056800040611     c
056900040610     c                   else
057000040610     c
057100040610     c* Se non devo creare e chainata, disalloco
057200040610     c                   unlock    fnanm02l
057300040610    3C                   ENDif
057400040609     c                   endsr
057500960202     C*
057600951115     C*--- SR INIZIALE -----------------------------------------------*
057700951115     C     *INZSR        BEGSR
057800040609     c
057900040609     C     *ENTRY        PLIST
058000040609     C                   PARM                    KPJBA
058100040609     c                   movel     kpjbu         para02
058200040609     C*
058300040609     C                   Z-ADD     1             CODUT
058400040609     C                   CALL      'X§PARUT'
058500040609     C                   PARM                    UT§DSE0F
058600040609     C                   MOVEL     REC80         CNCR80
058700040609     C                   MOVEL     RAGUT         RSUT             20
058800040607     c
058900040607     c* Carico i tipi traino defluenza
059000040607     c                   clear                   x
059100040607     C     KTAB          setll     TABEL
059200040607     C     KTAB          reade     TABEL
059300040607     c                   dow       not %eof(tabel00f)
059400040607     c                   if        tblflg=' '
059500040607     c                   movel     tbluni        dstv
059600040609     c                   if        §tvdef='S'
059700040607     c                   add       1             X                 3 0
059800040607     c                   movel     tblcod        tdef(x)
059900040607     c                   endif
060000040607     c                   endif
060100040607     C     KTAB          reade     TABEL
060200040607     c                   enddo
060300040609     c
060400040609     c
060500040609     c                   time                    w0140            14 0
060600040609     c                   move      w0140         udate8            8 0
060700040609     c                   movel     w0140         utime             6 0
060800040609     C* PRENDO FASE ANOMALIA
060900040609     C                   CLEAR                   PARAM5
061000040609     C                   MOVEL     'L'           PA5TLA
061100040609     C                   MOVEL     2             PA5NPG
061200040609     C                   CALL      'TRUL11R'
061300040609     C                   PARM                    PARAM5
061400040609     C*
061500040609     C                   MOVEL     '7C'          kCOD
061600040609     C                   MOVEL(P)  comCAA        kKEY
061700040609     C     KTAB2         CHAIN     TABEL00F                           35
061800040609     C  N35              MOVEL     TBLUNI        DS7C
061900040609     C   35              MOVEL     *BLANKS       DS7C
062000160920     c
062100160920     c* Verifico le richieste delle zone di consegna
062200160920     c                   clear                   wParc             1
062300160920     c                   clear                   wParc1            1
062400160920     c                   clear                   wParc2            1
062500160920     c                   clear                   wMess             1
062600160920     c                   clear                   wSelzona          1
062700160920     c                   if        p02tpm1='P ' or p02tpm2='P '
062800160920     c                   eval      wparc='S'
062900160920     c                   eval      wselzona='S'
063000160920     c                   endif
063100160920     c                   if        p02tpm1='M ' or p02tpm2='M '
063200160920     c                   eval      wMess='S'
063300160920     c                   eval      wselzona='S'
063400160920     c                   endif
063500160920     c                   if        p02tpm1='P1' or p02tpm2='P1'
063600160920     c                   eval      wParc1='S'
063700160920     c                   eval      wselzona='S'
063800160920     c                   endif
063900160920     c                   if        p02tpm1='P2' or p02tpm2='P2'
064000160920     c                   eval      wParc2='S'
064100160920     c                   eval      wselzona='S'
064200160920     c                   endif
064300160920     c
064400160920     c* se richiesta soltanto zone parcel o messagerie, carico in
064500160920     c*  schiera
064600160920     c                   if        wparc='S' and wmess='S'
064700160920     c                   clear                   wparc
064800160920     c                   clear                   wmess
064900160920     c                   clear                   wselzona
065000160920     c                   endif
065100160920     c
065200160920     c                   if        wparc1='S' and wparc2='S'
065300160920     c                   clear                   wparc1
065400160920     c                   clear                   wparc2
065500160920     c                   eval      wparc='S'
065600160920     c                   endif
065700160920     c
065800160920    1c                   if        wselzona='S'
065900160920     c                   clear                   xx                4 0
066000160920     c                   clear                   zz                4 0
066100160920     c                   movel     p02fgs        alffil            3
066200160920     c                   movel     p02fgs        wpo
066300160920     c                   movel(P)  '03'          kcod
066400160920     c                   movel(P)  p02fgs        kkey
066500160920     c     ktab2         setll     tabel
066600160920     c     ktab          reade     tabel
066700160920    2c                   dow       not %eof(tabel00f) and  wpo=alffil
066800160920    3c                   if        tblflg=' '
066900160920     c                   movel     tblkey        wpo               3
067000160920    4c                   if        wpo=alffil
067100160920     c                   movel     tbluni        ds03
067200160920
067300160920     c* parcel/messaggerie
067400160920    5c                   if        wparc='S' or wmess='S'
067500160920
067600160920    6c                   if        (wmess='S' and §03tpm='M') or
067700160920     c                             (wparc='S' and §03tpm<>'M')
067800160920     c                   add       1             xx
067900160920     c                   movel     tblkey        w005a             5
068000160920     c                   move      w005a         zone(xx)
068100160920    6c                   endif
068200160920    5c                   endif
068300160920     c* parcel 1° e 2° turno
068400160920    5c                   if        (wparc1='S' or wparc2='S')
068500160920     c                             and §03tpm<>'M'
068600160920
068700160920    6c                   if        (wparc2='S' and §03turno='2') or
068800160920     c                             (wparc1='S' and §03turno<>'2')
068900160920     c                   add       1             zz
069000160920     c                   movel     tblkey        w005a             5
069100160920     c                   move      w005a         zone12(zz)
069200160920    6c                   endif
069300160920    5c                   endif
069400160920     c
069500160920    4c                   endif
069600160920    3c                   endif
069700160920     c
069800160920     c     ktab          reade     tabel
069900160920    2c                   enddo
070000160920    1c                   endif
070100160920
070200160920     c                   clear                   statipo
070300160920     c                   clear                   statipo2
070400160920     c                   if        p02tpm1='P ' or p02tpm2='P '
070500160920     c                   eval      statipo=cparcel
070600160920     c                   endif
070700160920     c
070800160920     c                   if        p02tpm1='M ' or p02tpm2='M '
070900160920     c                   if        statipo=*blanks
071000160920     c                   eval      statipo=cmessag
071100160920     c                   else
071200160920     c                   eval      statipo2=cmessag
071300160920     c                   endif
071400160920     c                   endif
071500160920     c                   if        p02tpm1='P1' or p02tpm2='P1'
071600160920     c                   if        statipo=*blanks
071700160920     c                   eval      statipo=cparc1
071800160920     c                   else
071900160920     c                   eval      statipo2=cparc1
072000160920     c                   endif
072100160920     c                   endif
072200160920     c                   if        p02tpm1='P2' or p02tpm2='P2'
072300160920     c                   if        statipo=*blanks
072400160920     c                   eval      statipo=cparc2
072500160920     c                   else
072600160920     c                   eval      statipo2=cparc2
072700160920     c                   endif
072800160920     c                   endif
072900160920     C*
073000160920     c
073100951115     C*
073200951115     C****  KLIST  ****
073300040607     C     KTAB          KLIST
073400040607     C                   KFLD                    codut
073500040607     C                   KFLD                    kcod
073600040608     C     KTAB2         KLIST
073700040608     C                   KFLD                    codut
073800040608     C                   KFLD                    kcod
073900040608     C                   KFLD                    kkey
074000040607     C     KCAE          KLIST
074100040607     C                   KFLD                    KEPA
074200040608     C                   KFLD                    Welalna
074300040607     C     Kfogli        KLIST
074400040607     C                   KFLD                    caetfe
074500040607     C                   KFLD                    p02invdam
074600040607     C     Kbrv2         KLIST
074700040607     C                   KFLD                    knpg
074800040607     C                   KFLD                    knfv
074900040607     C                   KFLD                    kfgs
075000040607     C                   KFLD                    klna
075100951115     C*
075200040608     C     Kbrv7         KLIST
075300040608     C                   KFLD                    ter_brvlnp
075400040608     C                   KFLD                    TER_brvlna
075500040608     C                   KFLD                    TER_brvnrs
075600040608     C                   KFLD                    TER_brvnsc
075700040608     C     KFVV          KLIST
075800040608     C                   KFLD                    BRVNPG
075900040608     C                   KFLD                    WNFV5
076000040608     C                   KFLD                    BRVFGS
076100040608     C     Kar5          KLIST                                                  *
076200040608     C                   KFLD                    bltAAS                         *
076300040608     C                   KFLD                    bltLNP                         *
076400040608     C                   KFLD                    bltNRS                         *
076500040608     C                   KFLD                    bltNSP                         *
076600040608     C                   KFLD                    ktrd                           *
076700040608     C     Kblp          KLIST                                                  *
076800040608     C                   KFLD                    bltAAS                         *
076900040608     C                   KFLD                    bltLNP                         *
077000040608     C                   KFLD                    bltNRS                         *
077100040608     C                   KFLD                    bltNSP                         *
077200040609     C     KANM          KLIST                                                  FNANM02L
077300040609     C                   KFLD                    bltFLS                         *
077400040609     C                   KFLD                    blTLNA                         *
077500040609     C                   KFLD                    blTNRS                         *
077600040609     C                   KFLD                    blTNSC                         *
077700040609     C                   KFLD                    comCAA                         *
077800951115     C                   ENDSR
077900040604     C**************************************************************************
078000040608     OPRTF198   E            TES              02
078100040604     O                       STA(1)              66
078200040604     O                       STA(2)             132
078300040604     O                       STA(13)            198
078400040604     O                       RSUT                20
078500040604     O                       UDATE8             181 '  /  /    '
078600040604     O                       PAGE          Z    198
078700040604     O          E            TES         1
078800160920     o                       statipo             67
078900160920     o                       statipo2           130
079000040604     O                       UTIME              179 '  :  :  '
079100040604     O          E            TES         2
079200040604     O                       STA(3)              66
079300040609RM*  O                       p02FGS              20
079400040608     O                       DESFGS              41
079500040608     O                       p02dam        8     76
079600040604     O          E            TES2        2
079700040604     O                       STA(5)              66
079800040604     O                       STA(8)             132
079900040610     O                       STA(14)            198
080000040604     O          E            TES2        1  1
080100040604     O                       STA(6)              66
080200040604     O                       STA(9)             132
080300040610     O                       STA(15)            198
080400040604     O*
080500040604     O          E            DET         1
080600040604     O                  05   BLPAAS        Z
080700040604     O                  05   BLPLNP        Z   +  1
080800040604     O                  05 03BLPNRS        Z   +  1
080900040604     O                  05 03                  +  0 '-'
081000040604     O                  05   BLPNSP        Z   +  0
081100040604     O                  05   STADSP            +  1 '  /  '
081200040604     O                  05   BLPLNA        Z   +  1
081300040604     O                  05   STARSM            +  1
081400040604     O                  05   STARSD            +  1
081500040604     O                  05N04BLPNCD        Z   +  1
081600040604     O                  05N04                  +  0 '/'
081700040604     O                  05N04BLPNCA        Z   +  0
081800040604     O                  05 04                    84 'NON SEQUENZIALI'
081900040604     O                  05   BLPZNC            +  1
082000040604     O                       BRVLNP            +  4
082100040604     O                       BRVLNA            +  2
082200040604     O                       BRVNRS        Z   +  3
082300040604     O                       BRVNSC        Z   +  4
082400040617     O                       STADAN            +  5 '  /  /    '
082500040610     O                       STAOAN            +  2 '  :  '
082600040617     O                     13brvFGS             142
082700040617     O                     13                   143 '-'
082800040617     O                     13ST2FGS             151
082900070117     O                     13ST2CAT             155
083000070117     O                     13brvNPG             157
083100070117     O                     13                   158 '-'
083200070117     O                     13brvNFV        Z    164
083300070117     O                     13ST2DFV             171 '  /  '
083400070117     O                     13brvhms             180 '  :  :  '
083500040610     O                     13STADSF             198
083600040604     O          E            OK          3
083700040604     O                       STA(11)             66
083800040604     O          E            OK          0  0
083900040604     O                       STA(11)             66
084000040604     O**************************************************************************
084100040604** STA
084200040617XXXXXXXXXXXXXXXXXXXX                                                 1
084300040607 ** COLLI DI VALORE MANCANTI AL 2 LIVELLO IN ARRIVO **               2
084400040608PUNTO OPERATIVO: xxx-XXXXXXXXXXXXXXXXXXX      DATA ARRIVO COLLI:     3
084500040607
084600040604    S P E D I Z I O N E   LIN MITTENTE          DESTINATARIO         5
084700040604ANNO LNP SER NUMERO  DATA ARR                                        6
084800040604XXXX XXX XX XXXXXXX XX/XX XXX XXXXXXXXXXXXXXXXX xxxxxxxxxxxxxxxxxx   7
084900040617     SEGNACOLLI    ZON   C O L L O   M A N C A N T E  CREAZIONE AN
085000040617      DAL /   AL   ARR   LNP  LNA  SERIE   NUMERO        DATA
085100040617XX XXXXXXX/XXXXXXX XX    XXX  XXX   XX    XXXXXXX     XX/XX/XXXx
085200040604             TUTTO  O.K. : NESSUNA ANOMALIA RISCONTRATA             11
085300040604 **  FINE  STAMPA  **                                               12
085400040607                         FNLRV2R                          PAG.XXXX  13
085500040617OMALIA   S  P   U   N   T   A   T   O      D  A
085600070117 ORA   P.Operativo    Foglio       del   Ore
085700070117xx:xx  XXX-XXXXXXXX ARR 2-XXXXXX  XX/XX xx:xx:xx  xxxxxxxxxxxxxxxxx
085800040604**
085900040604OVRPRTF FILE(PRTF198) USRDTA('ColValore')
