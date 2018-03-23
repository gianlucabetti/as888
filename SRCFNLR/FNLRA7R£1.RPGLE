000100941214     H DECEDIT('0,') DATEDIT(*DMY.)
000200941214     F* FNLRA7R *-----------------------------------------------------*
000300941214     F*   PULIZIA F.VIAGGIO / FOGLI VARI / F.VIAGGIO PART. IN ARRIVO  *
000400921111     F*---------------------------------------------------------------*
000500941214     FFNFVV02L  UF   E           K DISK
000600941214     FFNFGV02L  UF   E           K DISK
000700000502     FFNFGW01L  UF   E           K DISK
000800000223     FFNFVA03L  UF   E           K DISK
000900000502     FFNFWA01L  UF   E           K DISK
001000000223     FFNFVA01L  IF   E           K DISK
001100000223     F                                     RENAME(FNFVA000:FNFVA001)
001200070118     FFNBRV01L  IF   E           K DISK
001300941214     FFNFVD01L  UF   E           K DISK
001400950616     FFNDST01L  UF   E           K DISK
001500070419     FFidst09l  UF   E           K DISK    prefix(£)
001600070419     FFidst31l  UF   E           K DISK
001700070725     FFidst41l  UF   E           K DISK
001800070725     FFidst51l  UF   E           K DISK
001900070725     FFidst61l  UF   E           K DISK
002000080725     FFidsf01l  UF   E           K DISK
002100951207     FFNFV201L  UF   E           K DISK
002200951207     FFNFV401L  UF   E           K DISK
002300020809     FFNBLP79L  IF   E           K DISK
002400921111     FTABEL00F  IF   E           K DISK
002500021104     fFnfgv01l  if   e           k Disk
002600021104     f                                     Rename(Fnfgv000:fnfgv01)
002700941215     D*
002800941215     D* DEFINIZIONE SCHIERE
002900941215     D*
003000941215     D*LINEE   DI ARRIVO
003100941215     D                 DS
003200941215     D  FVALNA                 1      3  0
003300941215     D  FVAFFV                 4    243
003400941215     D  FVAFF2               244    450
003500000502     D  FWAFF3               451    690
003600000502     D  FWAFF4               691    900
003700000502     D  VUOTO                901    903
003800000502     D  FFV                    1    903
003900000502     D                                     DIM(301)                             LINEE FOGLIO
004000941215     D*FILIALI DI SCARICO
004100941215     D                 DS
004200941215     D  LNA2                   1      3  0
004300941215     D  FVAFLP                 4    243
004400941215     D  FVAFL2               244    450
004500000502     D  FWAFL3               451    690
004600000502     D  FWAFL4               691    900
004700000502     D  VUOTO2               901    903
004800000502     D  FLP                    1    903
004900000502     D                                     DIM(301)                             LINEE TRANSITO
005000921111     D*
005100020809     D KPJBA         E DS
005200020809     D UT§DSE0F      E DS
005300020809     D CNCR80        E DS
005400020809     D DS7N          E DS
005500921111     D*
005600921111     D PARAM           DS
005700941215     D* DATA PULIZIA F.VIAGGIO PARTENZA
005800941215     D  COMFGV                35     42  0
005900941215     D* DATA PULIZIA FOGLI VARI
006000941215     D  COMFVV                43     50  0
006100941215     D* DATA PULIZIA F.VIAGGIO PARTENZA IN ARRIVO
006200941215     D  COMFVA                51     58  0
006300020809      *
006400020809      * Variabili
006500020809      *
006600020809     D KFGS            S                   LIKE(FVVFGS)
006700020809     D KNPG            S                   LIKE(FVVNPG)
006800020809     D KNFV            S                   LIKE(FVVNFV)
006900020809     D COD             S                   LIKE(TBLCOD)
007000020809     D KNFVS           S                   LIKE(BRVNFV)
007100050103     D Klnp            S                   LIKE(fgvlnp)
007200070419     d wnfv            s                   like(£dstnfv)
007300921111     C*---------------------------------------------------------------*
007400921111     C*  INDICATORI USATI                                             *
007500921111     C*---------------------------------------------------------------*
007600941215     C* 01    - DELETO LA SPUNTA
007700941215     C* 30/36 - DI COMODO
007800921111     C*---------------------------------------------------------------*
007900930216     C*
008000930216     C* DEFINIZIONE VARIABILI E KLIST
008100921111     C                   EXSR      DEFVAR
008200921111     C*
008300921112     C                   MOVEL     '7N'          COD
008400941215     C     KTAB          SETLL     TABEL00F
008500941215     C     KTAB          READE     TABEL00F                               30
008600930216     C*
008700941215    1C     *IN30         DOWEQ     *OFF
008800921111     C                   MOVEL     TBLUNI        DS7N
008900941215     C*
009000941214     C****
009100941214     C** P U L I Z I A   F O G L I   V A R I
009200941214     C****
009300941215    2C     §7NFFV        IFEQ      'S'
009400941214     C*
009500921111     C                   MOVEL     TBLKEY        KNPG
009600941214     C     KNPG          SETLL     FNFVV02L
009700941214     C     KNPG          READE     FNFVV02L                               31
009800921111     C*
009900941215    3C     *IN31         DOWEQ     *OFF
010000941215     C     FVVDFV        ANDLE     COMFVV
010100921111     C*
010200941229     C* FOGLIO CHIUSO = "S"
010300100705    4C**   FVVFCF        IFEQ      'S'
010400930930     C*
010500930930     C* OPPURE FOGLIO APERTO MA ANNULLATO
010600100705     C**   FVVFCF        ORNE      'S'
010700100705     C**   FVVATB        ANDNE     *BLANKS
010800930216     C*
010900930930     C* SE NON CI SONO SPUNTE ASSOCIATE CANCELLO
011000100705     c*  sempre
011100921111     C                   Z-ADD     FVVNFV        KNFV
011200000207     C                   Z-ADD     FVVNFV        KNFVS
011300941229     C                   Z-ADD     FVVFGS        KFGS
011400070118     C     KEYBRV        SETLL     FNBRV01L                               32
011500950616     C*
011600950616    5C     *IN32         IFEQ      *OFF
011700950616     C* SE CATEGORIA = "4" CANCELLO RECORD IN FNDST00F SE ESISTE
011800950616    6C     KNPG          IFEQ      4
011900950616     C     KDST          CHAIN     FNDST000                           34
012000950616     C  N34              DELETE    FNDST000
012100070419     C* CANCELLO RECORD IN FIDST00F SE ESISTE e estensione giri
012200070419     c                   eval      wnfv = fvvnfv
012300070419     c     kdst2         chain     fidst09l
012400070419     c                   if        %found(fidst09l) and
012500070419     c                             (£dstfcf = 'S' or (£dstfcf <> 'S' and
012600070419     c                             £dstatb <>' '))
012700070725     c*
012800070419     c     kdst3         setll     fidst31l
012900070419     c                   do        *hival
013000070419     c     kdst3         reade     fidst31l
013100070419     c                   if        %eof(fidst31l)
013200070419     c                   leave
013300070419     c                   end
013400070419     c                   delete    fidst300
013500070419     c                   enddo
013600070725     c*
013700070725     c     kdst2         setll     fidst41l
013800070725     c                   do        *hival
013900070725     c     kdst2         reade     fidst41l
014000070725     c                   if        %eof(fidst41l)
014100070725     c                   leave
014200070725     c                   end
014300070725     c                   delete    fidst400
014400070725     c                   enddo
014500070725     c*
014600070725     c     kdst3         setll     fidst51l
014700070725     c                   do        *hival
014800070725     c     kdst3         reade     fidst51l
014900070725     c                   if        %eof(fidst51l)
015000070725     c                   leave
015100070725     c                   end
015200070725     c                   delete    fidst500
015300070725     c                   enddo
015400070725     c*
015500070725     c     kdst2         setll     fidst61l
015600070725     c                   do        *hival
015700070725     c     kdst2         reade     fidst61l
015800070725     c                   if        %eof(fidst61l)
015900070725     c                   leave
016000070725     c                   end
016100070725     c                   delete    fidst600
016200070725     c                   enddo
016300080725     c*
016400080725     c     kdst4         setll     fidsf01l
016500080725     c                   do        *hival
016600080725     c     kdst4         reade     fidsf01l
016700080725     c                   if        %eof(fidsf01l)
016800080725     c                   leave
016900080725     c                   end
017000080725     c                   delete    fidsf000
017100080725     c                   enddo
017200080725     C                   DELETE    FiDST000
017300080725     c                   end
017400950616    6C                   ENDIF
017500950616     C*
017600950616     C                   DELETE    FNFVV02L
017700100705     c
017800100705     c*
017900100705   x5c                   else
018000100705     c* se il foglio ha spunte ed è annullato, lo considero valido e lo
018100100705     c*  chiudo per permetterne la pulizia al momento giusto
018200100705    6c                   if        fvvatb<>' '
018300100705     c                   eval      fvvatb=' '
018400100705     c                   eval      fvvfcf='S'
018500100706     c                   eval      fvvdsf='Foglio Annullato chiuso da pulizie'
018600100705     c                   update    fnfvv000
018700100705    6c                   endif
018800100705     c*
018900100705     c* Se il foglio non è annnullato e aperto --> non chiudo le cat 4 e 8
019000100705     c                   if        fvvfcf=' ' and fvvnpg<>4 and fvvnpg<>8
019100100705     c                   eval      fvvdsf='CHIUSURA AUTOMATICA da pulizie'
019200100705     c                   eval      fvvfcf='S'
019300100705     c                   update    fnfvv000
019400100705    6c                   endif
019500100705     c
019600950616    5C                   ENDIF
019700100705    4C*****              ENDIF
019800921111     C*
019900941214     C     KNPG          READE     FNFVV02L                               31
020000941215    3C                   ENDDO
020100921111     C*
020200941214     C*
020300941215    2C                   ELSE
020400941214     C****
020500941214     C** P U L I Z I A   F O G L I   V I A G G I O   P A R T E N Z E
020600941214     C****
020700960214     C                   Z-ADD     1             KNPG                           *CATEG.FOGLI
020800960214     C*
020900020809     C     *LOVAL        SETLL     FNFGV02L
021000020809     C                   READ      FNFGV02L                               33
021100921111     C*
021200941215    3C     *IN33         DOWEQ     *OFF
021300050103     c
021400050103     c* Elaboro tutte le linee di partenza
021500050103     c                   z-add     fgvlnp        klnp
021600050103     C     klnp          SETLL     FNFGV02L
021700050103     C     klnp          READe     FNFGV02L                               33
021800050103     c*
021900050103   3aC     *IN33         DOWEQ     *OFF
022000941215     C     FGVDFV        ANDLE     COMFGV
022100921111     C*
022200921111     C* FOGLIO CHIUSO
022300941215    4C     FGVFCF        IFNE      *BLANKS
022400000502     C     FGVATB        ORNE      ' '
022500930216     C*
022600930930     C* CONTROLLO SE NON CI SONO SPUNTE ASSOCIATE
022700000207     C                   Z-ADD     FGVNFV        KNFVS
022800941229     C                   Z-ADD     FGVLNP        KFGS
022900100705     C     KEYBRV        SETLL     FNBRV01L                               34
023000930216     C*
023100960214    5C     *IN34         IFEQ      *OFF
023200930216     C* E SE NON CI SONO BOLLE: CANCELLO
023300020809     ***C     FGVNFV        SETLL     FNBLP29L                               35
023400020809     C     KFVD          setll     FNBLP79L                               35
023500930716     C*
023600941215    6C     *IN35         IFEQ      *OFF
023700930928     C*
023800941214     C* SE ESISTONO RECORD IN FNFVD LI DELETO
023900941214     C     KFVD          SETLL     FNFVD000
024000000502     C                   SETOFF                                       3601
024100941215    7C     *IN36         DOWEQ     *OFF
024200000502     C     *IN01         ANDEQ     *OFF
024300000502     C     KFVD          READE     FNFVD000                             0136
024400000502     C  N01
024500000502     CANN36              DELETE    FNFVD000
024600941215    7C                   ENDDO
024700930716     C*
024800951207     C* SE ESISTONO RECORD IN FNFV2/FNFV4 LI DELETO
024900021104      * da 04/11/02 solo per fv2
025000000502     C  N01              EXSR      FV2FV4
025100000502     C* SE ESISTE FNGW LO DELETO
025200020809     ***C  N01FGVNFV        DELETE    FNFGW000                           3301
025300020809     C  N01K02FGW01      delete    FNFGW000                           3301
025400951207     C*
025500941214     C  N01              DELETE    FNFGV02L
025600941215    6C                   ENDIF
025700930716     C*
025800941215    5C                   ENDIF
025900020809     C*
026000941215    4C                   ENDIF
026100921112     C*
026200050103     C     klnp          READE     FNFGV02L                               33
026300050103   3AC                   ENDDO
026400050103     C*
026500050103     C     klnp          SETGT     FNFGV02L
026600050103     C                   READ      FNFGV02L                               33
026700050103    3C                   ENDDO
026800050103     C
026900921111     C*
027000941214     C****
027100941214     C** P U L I Z I A   F O G L I   V I A G G I O   P A R T E N Z E
027200941214     C** I N   A R R I V O
027300941214     C****
027400941215     C                   Z-ADD     0             FIL               3 0
027500941215     C                   SETOFF                                       33
027600941215     C*
027700941215    3C     *IN33         DOWEQ     *OFF
027800000223     C     FIL           SETGT     FNFVA03L
027900000223     C                   READ      FNFVA03L                               33
028000941215     C*
028100941215    4C     *IN33         IFEQ      *OFF
028200941215     C                   Z-ADD     FVALNP        FIL
028300941215     C*
028400941215     C     FIL           SETLL     FNFVA000
028500941215     C     FIL           READE     FNFVA000                               31
028600941215     C*
028700941215    5C     *IN31         DOWEQ     '0'
028800941215     C*
028900941215     C* SE DATA FOGLIO VIAGGIO > DATA PULIZIA LEGGO ALTRA LINEA
029000941215    6C     FVADFV        IFGT      COMFVA
029100941215     C                   SETON                                        31
029200941215   X6C                   ELSE
029300941215     C                   SETOFF                                       01
029400941215     C* CONTROLLO SE ESISTONO SPUNTE SU QUEL FOGLIO
029500941215     C                   EXSR      PULSPU
029600941215     C*
029700941215     C* SE TUTTO O.K. CANCELLO FV
029800000502     C  N36
029900000502     CANN01              DELETE    FNFWA000
030000941215     C  N01              DELETE    FNFVA000
030100941215     C*
030200941215     C     FIL           READE     FNFVA000                               31
030300941215    6C                   ENDIF
030400941215    5C                   ENDDO
030500941215     C*
030600941215    4C                   ENDIF
030700941215    3C                   ENDDO
030800021104     C****
030900021104     C** P U L I Z I A   F V 4
031000021104     C****
031100941215     C*
031200960214     C* CANCELLO FV4 SOLO SE NON ESISTE PIU' FNFVA
031300021104      * e FnFgv
031400960214     C     *LOVAL        SETLL     FNFV401L
031500960214     C                   READ      FNFV401L                             0136
031600960214    3C     *IN36         DOWEQ     *OFF
031700960214     C     *IN01         ANDEQ     *OFF
031800021104      * Per prima cosa devo controllare che non esista FGV
031900021104     c     Kfgv          setll     Fnfgv01l
032000021104If  4c                   If        Not %Equal(Fnfgv01l)
032100021104      * Poi controllo che non esista FVA
032200021104If  5c                   If        Fv4Lai > *Zeros
032300021104     c     Kfva          Setll     Fnfva01l
032400021104   x5c                   Else
032500021104     c     Kfva1         Setll     Fnfva01l
032600021104     c                   EndIf
032700021104If  5c                   If        Not %Equal(Fnfva01l)
032800960214     C* SE NON TROVATO CANCELLO
032900021104     C                   DELETE    FNFV4000
033000021104    5C                   ENDIF
033100021104    4C                   ENDIF
033200960314     C                   READ      FNFV401L                             0136
033300960214    3C                   ENDDO
033400960214     C*
033500960214    2C                   ENDIF
033600941215     C*
033700941215     C     KTAB          READE     TABEL00F                               30
033800941215    1C                   ENDDO
033900921111     C*
034000921111     C                   SETON                                        LR
034100941215     C*---------------------------------------------------------------*
034200941215     C*                    PULSPU                                     *
034300941215     C* ROUTINE DI CONTROLLO ESISTENZA SPUNTE PER QUEL FOGLIO         *
034400941215     C*---------------------------------------------------------------*
034500941215     C     PULSPU        BEGSR
034600941215     C*
034700000502     C     KFWA          CHAIN     FNFWA000                           36
034800000502     C   36              CLEAR                   FWAFF3
034900000502     C   36              CLEAR                   FWAFF4
035000000502     C   36              CLEAR                   FWAFL3
035100000502     C   36              CLEAR                   FWAFL4
035200000502     C*
035300000207     C                   Z-ADD     FVANFV        KNFVS
035400000502     C                   Z-ADD     FVALNP        KFGS
035500000502     C                   Z-ADD     FVALNA        LNA2
035600070118     C     KEYBRV        SETLL     FNBRV01L
035700070118     C     KEYBRV        READE     FNBRV01L                               35
035800941215     C*
035900941215    1C     *IN35         DOWEQ     *OFF
036000941215     C*
036100941215     C* UNA SPUNTA APPARTIENE A UN FOGLIO DI FNFVA:
036200941215     C*
036300941215     C*  - SE LINEA ARRIVO SPUNTA = LINEA INVIO FOGLIO
036400941215    2C     BRVLNA        IFEQ      FVALAI
036500941215     C                   SETON                                        3501
036600941215    2C                   ENDIF
036700941215     C*
036800941215     C*  - SE FIL.SCARICO DELLA LNA SPUNTA = LINEA INVIO FOGLIO
036900941215    2C     *IN35         IFEQ      *OFF
037000941215     C*
037100941215     C                   Z-ADD     1             X                 3 0
037200941215     C                   MOVEL     BRVLNA        WLNA              3
037300941215     C                   MOVEL     FVALAI        WLAI              3
037400941215     C     WLNA          LOOKUP    FFV(X)                                 20
037500941215    3C   20FLP(X)        IFEQ      WLAI
037600941215     C                   SETON                                        3501
037700941215    3C                   ENDIF
037800941215     C*
037900941215     C*  - SE TERMINAL ARRIVO DELLA LNA SPUNTA = LINEA INVIO FOGLIO
038000000502     C*    QUESTO 3° CASO PER LE SPUNTE NON ACCADE PERCHE' LE SPUNTE
038100000502     C*    INVIATE VENGONO IMMEDIATAENTE CANCELLATE DOPO L'AGGIORNAMENT
038200000502    2C                   ENDIF
038300941215     C*
038400070118     C  N35KEYBRV        READE     FNBRV01L                               35
038500941215    1C                   ENDDO
038600941215     C*
038700941215     C                   ENDSR
038800951207     C*---------------------------------------------------------------*
038900951207     C*                    FV2FV4                                     *
039000951207     C* ROUTINE DI PULIZIA FNFV200F E FNFV400F                        *
039100951207     C*---------------------------------------------------------------*
039200951207     C     FV2FV4        BEGSR
039300951207     C*
039400960214     C     KFVD          CHAIN     FNFV201L                           3601
039500960214    1C     *IN36         DOWEQ     *OFF
039600960214     C     *IN01         ANDEQ     *OFF
039700960214     C                   DELETE    FNFV2000
039800960214     C*
039900960214     C     KFVD          READE     FNFV201L                             0136
040000960214    1C                   ENDDO
040100951207     C*
040200021104    1C**!!!*IN01         IFEQ      *OFF
040300960214     C**
040400021104     C**!!!              SETOFF                                       37
040500021104     C**!!!KFVD          SETLL     FNFV401L
040600021104    2C**!!!*IN37         DOWEQ     *OFF
040700021104     C**!!!*IN01         ANDEQ     *OFF
040800021104     C**!!!KFVD          READE     FNFV401L                             0137
040900021104     C**!!!  N01
041000021104     C**!!!ANN37              DELETE    FNFV4000
041100021104    2C**!!!              ENDDO
041200021104    1C**!!!              ENDIF
041300951207     C*
041400951207     C                   ENDSR
041500921111     C*---------------------------------------------------------------*
041600921111     C*                    DEFVAR                                     *
041700921111     C*  ROUTINE DI DEFINIZIONE KEYLIST E VARIABILI                   *
041800921111     C*---------------------------------------------------------------*
041900921111     C     DEFVAR        BEGSR
042000921111     C*
042100930216     C     *ENTRY        PLIST
042200930216     C                   PARM                    KPJBA
042300930216     C                   MOVEL     KPJBU         PARAM
042400930216     C*
042500020809     ***C                   Z-ADD     1             CODUT
042600960214     C                   CALL      'X§PARUT'
042700020809     C                   PARM                    UT§DSE0F
042800960214     C                   MOVEL     REC80         CNCR80
042900930216     C*
043000921111     C* AGGANCIO FILE TABEL00F
043100941215     C     KTAB          KLIST
043200921111     C                   KFLD                    CODUT
043300921111     C                   KFLD                    COD
043400921111     C*
043500070118     C* AGGANCIO FILE FNBRV01L
043600921111     C     KEYBRV        KLIST
043700921111     C                   KFLD                    KNPG
043800000207     C                   KFLD                    KNFVS
043900941229     C                   KFLD                    KFGS
044000921111     C*
044100941215     C* AGGANCIO FILE FNFVD01L
044200930716     C     KFVD          KLIST
044300930716     C                   KFLD                    FGVLNP
044400930716     C                   KFLD                    FGVNFV
044500000502     C     KFWA          KLIST
044600000502     C                   KFLD                    FVALNP
044700000502     C                   KFLD                    FVANFV
044800000502     C                   KFLD                    FVALAI
044900020809     C*
045000020809     C* Aggancio file FNFGW01L
045100020809     C     K02FGW01      klist
045200020809     C                   kfld                    FGVnfv
045300020809     C                   kfld                    FGVlnp
045400950616     C*
045500950616     C* AGGANCIO FILE FNDST01L
045600950616     C     KDST          KLIST
045700950616     C                   KFLD                    KNFV
045800950616     C                   KFLD                    KFGS
045900070419     C     Kdst2         KLIST
046000070419     C                   KFLD                    fvvFGS
046100070419     C                   KFLD                    fvvnpg
046200070419     C                   KFLD                    wnfv
046300070419     C     Kdst3         KLIST
046400070419     C                   KFLD                    fvvFGS
046500070419     C                   KFLD                    wnfv
046600080725     C     Kdst4         KLIST
046700080725     C                   KFLD                    fvvnpg
046800080725     C                   KFLD                    wnfv
046900080725     C                   KFLD                    fvvFGS
047000930930     C*
047100021104      * Fnfgv01
047200021104     c     Kfgv          Klist
047300021104     c                   Kfld                    Fv4Nfv
047400021104     c                   Kfld                    Fv4Lnp
047500021104      * Fnfva01
047600021104     c     Kfva          Klist
047700021104     c                   Kfld                    Fv4Lnp
047800021104     c                   Kfld                    Fv4Nfv
047900021104     c                   Kfld                    Fv4Lai
048000021104     c     Kfva1         KLIST
048100021104     C                   KFLD                    FV4LNP
048200021104     C                   KFLD                    FV4NFV
048300020809     ***C*
048400020809     ***C     *LIKE         DEFINE    FVVFGS        KFGS
048500020809     ***C     *LIKE         DEFINE    FVVNPG        KNPG
048600020809     ***C     *LIKE         DEFINE    FVVNFV        KNFV
048700020809     ***C     *LIKE         DEFINE    TBLCOD        COD
048800020809     ***C     *LIKE         DEFINE    BRVNFV        KNFVS
048900930716     C*
049000921111     C                   ENDSR
