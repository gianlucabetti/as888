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
001400070419     FFidst09l  UF   E           K DISK    prefix(£)
001500070419     FFidst31l  UF   E           K DISK
001600070725     FFidst41l  UF   E           K DISK
001700070725     FFidst51l  UF   E           K DISK
001800070725     FFidst61l  UF   E           K DISK
001900080725     FFidsf01l  UF   E           K DISK
002000951207     FFNFV201L  UF   E           K DISK
002100951207     FFNFV401L  UF   E           K DISK
002200020809     FFNBLP79L  IF   E           K DISK
002300921111     FTABEL00F  IF   E           K DISK
002400021104     fFnfgv01l  if   e           k Disk
002500021104     f                                     Rename(Fnfgv000:fnfgv01)
002600941215     D*
002700941215     D* DEFINIZIONE SCHIERE
002800941215     D*
002900941215     D*LINEE   DI ARRIVO
003000941215     D                 DS
003100941215     D  FVALNA                 1      3  0
003200941215     D  FVAFFV                 4    243
003300941215     D  FVAFF2               244    450
003400000502     D  FWAFF3               451    690
003500000502     D  FWAFF4               691    900
003600000502     D  VUOTO                901    903
003700000502     D  FFV                    1    903
003800000502     D                                     DIM(301)                             LINEE FOGLIO
003900941215     D*FILIALI DI SCARICO
004000941215     D                 DS
004100941215     D  LNA2                   1      3  0
004200941215     D  FVAFLP                 4    243
004300941215     D  FVAFL2               244    450
004400000502     D  FWAFL3               451    690
004500000502     D  FWAFL4               691    900
004600000502     D  VUOTO2               901    903
004700000502     D  FLP                    1    903
004800000502     D                                     DIM(301)                             LINEE TRANSITO
004900921111     D*
005000020809     D KPJBA         E DS
005100020809     D UT§DSE0F      E DS
005200020809     D CNCR80        E DS
005300020809     D DS7N          E DS
005400921111     D*
005500921111     D PARAM           DS
005600941215     D* DATA PULIZIA F.VIAGGIO PARTENZA
005700941215     D  COMFGV                35     42  0
005800941215     D* DATA PULIZIA FOGLI VARI
005900941215     D  COMFVV                43     50  0
006000941215     D* DATA PULIZIA F.VIAGGIO PARTENZA IN ARRIVO
006100941215     D  COMFVA                51     58  0
006200020809      *
006300020809      * Variabili
006400020809      *
006500020809     D KFGS            S                   LIKE(FVVFGS)
006600020809     D KNPG            S                   LIKE(FVVNPG)
006700020809     D KNFV            S                   LIKE(FVVNFV)
006800020809     D COD             S                   LIKE(TBLCOD)
006900020809     D KNFVS           S                   LIKE(BRVNFV)
007000050103     D Klnp            S                   LIKE(fgvlnp)
007100070419     d wnfv            s                   like(£dstnfv)
007200921111     C*---------------------------------------------------------------*
007300921111     C*  INDICATORI USATI                                             *
007400921111     C*---------------------------------------------------------------*
007500941215     C* 01    - DELETO LA SPUNTA
007600941215     C* 30/36 - DI COMODO
007700921111     C*---------------------------------------------------------------*
007800930216     C*
007900930216     C* DEFINIZIONE VARIABILI E KLIST
008000921111     C                   EXSR      DEFVAR
008100921111     C*
008200921112     C                   MOVEL     '7N'          COD
008300941215     C     KTAB          SETLL     TABEL00F
008400941215     C     KTAB          READE     TABEL00F                               30
008500930216     C*
008600941215    1C     *IN30         DOWEQ     *OFF
008700921111     C                   MOVEL     TBLUNI        DS7N
008800941215     C*
008900941214     C****
009000941214     C** P U L I Z I A   F O G L I   V A R I
009100941214     C****
009200941215    2C     §7NFFV        IFEQ      'S'
009300941214     C*
009400921111     C                   MOVEL     TBLKEY        KNPG
009500941214     C     KNPG          SETLL     FNFVV02L
009600941214     C     KNPG          READE     FNFVV02L                               31
009700921111     C*
009800941215    3C     *IN31         DOWEQ     *OFF
009900941215     C     FVVDFV        ANDLE     COMFVV
010000921111     C*
010100941229     C* FOGLIO CHIUSO = "S"
010200100705    4C**   FVVFCF        IFEQ      'S'
010300930930     C*
010400930930     C* OPPURE FOGLIO APERTO MA ANNULLATO
010500100705     C**   FVVFCF        ORNE      'S'
010600100705     C**   FVVATB        ANDNE     *BLANKS
010700930216     C*
010800930930     C* SE NON CI SONO SPUNTE ASSOCIATE CANCELLO
010900100705     c*  sempre
011000921111     C                   Z-ADD     FVVNFV        KNFV
011100000207     C                   Z-ADD     FVVNFV        KNFVS
011200941229     C                   Z-ADD     FVVFGS        KFGS
011300070118     C     KEYBRV        SETLL     FNBRV01L                               32
011400950616     C*
011500950616    5C     *IN32         IFEQ      *OFF
011600070419     C* CANCELLO RECORD IN FIDST00F SE ESISTE e estensione giri
011700070419     c                   eval      wnfv = fvvnfv
011800070419     c     kdst2         chain     fidst09l
011900070419     c                   if        %found(fidst09l) and
012000070419     c                             (£dstfcf = 'S' or (£dstfcf <> 'S' and
012100070419     c                             £dstatb <>' '))
012200070725     c*
012300070419     c     kdst3         setll     fidst31l
012400070419     c                   do        *hival
012500070419     c     kdst3         reade     fidst31l
012600070419     c                   if        %eof(fidst31l)
012700070419     c                   leave
012800070419     c                   end
012900070419     c                   delete    fidst300
013000070419     c                   enddo
013100070725     c*
013200070725     c     kdst2         setll     fidst41l
013300070725     c                   do        *hival
013400070725     c     kdst2         reade     fidst41l
013500070725     c                   if        %eof(fidst41l)
013600070725     c                   leave
013700070725     c                   end
013800070725     c                   delete    fidst400
013900070725     c                   enddo
014000070725     c*
014100070725     c     kdst3         setll     fidst51l
014200070725     c                   do        *hival
014300070725     c     kdst3         reade     fidst51l
014400070725     c                   if        %eof(fidst51l)
014500070725     c                   leave
014600070725     c                   end
014700070725     c                   delete    fidst500
014800070725     c                   enddo
014900070725     c*
015000070725     c     kdst2         setll     fidst61l
015100070725     c                   do        *hival
015200070725     c     kdst2         reade     fidst61l
015300070725     c                   if        %eof(fidst61l)
015400070725     c                   leave
015500070725     c                   end
015600070725     c                   delete    fidst600
015700070725     c                   enddo
015800080725     c*
015900080725     c     kdst4         setll     fidsf01l
016000080725     c                   do        *hival
016100080725     c     kdst4         reade     fidsf01l
016200080725     c                   if        %eof(fidsf01l)
016300080725     c                   leave
016400080725     c                   end
016500080725     c                   delete    fidsf000
016600080725     c                   enddo
016700080725     C                   DELETE    FiDST000
016800080725     c                   end
016900950616     C*
017000950616     C                   DELETE    FNFVV02L
017100100705     c
017200100705     c*
017300100705   x5c                   else
017400100705     c* se il foglio ha spunte ed è annullato, lo considero valido e lo
017500100705     c*  chiudo per permetterne la pulizia al momento giusto
017600100705    6c                   if        fvvatb<>' '
017700100705     c                   eval      fvvatb=' '
017800100705     c                   eval      fvvfcf='S'
017900100706     c                   eval      fvvdsf='Foglio Annullato chiuso da pulizie'
018000100705     c                   update    fnfvv000
018100100705    6c                   endif
018200100705     c*
018300100705     c* Se il foglio non è annnullato e aperto --> non chiudo le cat 4 e 8
018400100705     c                   if        fvvfcf=' ' and fvvnpg<>4 and fvvnpg<>8
018500100705     c                   eval      fvvdsf='CHIUSURA AUTOMATICA da pulizie'
018600100705     c                   eval      fvvfcf='S'
018700100705     c                   update    fnfvv000
018800100705    6c                   endif
018900100705     c
019000950616    5C                   ENDIF
019100100705    4C*****              ENDIF
019200921111     C*
019300941214     C     KNPG          READE     FNFVV02L                               31
019400941215    3C                   ENDDO
019500921111     C*
019600941214     C*
019700941215    2C                   ELSE
019800941214     C****
019900941214     C** P U L I Z I A   F O G L I   V I A G G I O   P A R T E N Z E
020000941214     C****
020100960214     C                   Z-ADD     1             KNPG                           *CATEG.FOGLI
020200960214     C*
020300020809     C     *LOVAL        SETLL     FNFGV02L
020400020809     C                   READ      FNFGV02L                               33
020500921111     C*
020600941215    3C     *IN33         DOWEQ     *OFF
020700050103     c
020800050103     c* Elaboro tutte le linee di partenza
020900050103     c                   z-add     fgvlnp        klnp
021000050103     C     klnp          SETLL     FNFGV02L
021100050103     C     klnp          READe     FNFGV02L                               33
021200050103     c*
021300050103   3aC     *IN33         DOWEQ     *OFF
021400941215     C     FGVDFV        ANDLE     COMFGV
021500921111     C*
021600921111     C* FOGLIO CHIUSO
021700941215    4C     FGVFCF        IFNE      *BLANKS
021800000502     C     FGVATB        ORNE      ' '
021900930216     C*
022000930930     C* CONTROLLO SE NON CI SONO SPUNTE ASSOCIATE
022100000207     C                   Z-ADD     FGVNFV        KNFVS
022200941229     C                   Z-ADD     FGVLNP        KFGS
022300100705     C     KEYBRV        SETLL     FNBRV01L                               34
022400930216     C*
022500960214    5C     *IN34         IFEQ      *OFF
022600930216     C* E SE NON CI SONO BOLLE: CANCELLO
022700020809     ***C     FGVNFV        SETLL     FNBLP29L                               35
022800020809     C     KFVD          setll     FNBLP79L                               35
022900930716     C*
023000941215    6C     *IN35         IFEQ      *OFF
023100930928     C*
023200941214     C* SE ESISTONO RECORD IN FNFVD LI DELETO
023300941214     C     KFVD          SETLL     FNFVD000
023400000502     C                   SETOFF                                       3601
023500941215    7C     *IN36         DOWEQ     *OFF
023600000502     C     *IN01         ANDEQ     *OFF
023700000502     C     KFVD          READE     FNFVD000                             0136
023800000502     C  N01
023900000502     CANN36              DELETE    FNFVD000
024000941215    7C                   ENDDO
024100930716     C*
024200951207     C* SE ESISTONO RECORD IN FNFV2/FNFV4 LI DELETO
024300021104      * da 04/11/02 solo per fv2
024400000502     C  N01              EXSR      FV2FV4
024500000502     C* SE ESISTE FNGW LO DELETO
024600020809     ***C  N01FGVNFV        DELETE    FNFGW000                           3301
024700020809     C  N01K02FGW01      delete    FNFGW000                           3301
024800951207     C*
024900941214     C  N01              DELETE    FNFGV02L
025000941215    6C                   ENDIF
025100930716     C*
025200941215    5C                   ENDIF
025300020809     C*
025400941215    4C                   ENDIF
025500921112     C*
025600050103     C     klnp          READE     FNFGV02L                               33
025700050103   3AC                   ENDDO
025800050103     C*
025900050103     C     klnp          SETGT     FNFGV02L
026000050103     C                   READ      FNFGV02L                               33
026100050103    3C                   ENDDO
026200050103     C
026300921111     C*
026400941214     C****
026500941214     C** P U L I Z I A   F O G L I   V I A G G I O   P A R T E N Z E
026600941214     C** I N   A R R I V O
026700941214     C****
026800941215     C                   Z-ADD     0             FIL               3 0
026900941215     C                   SETOFF                                       33
027000941215     C*
027100941215    3C     *IN33         DOWEQ     *OFF
027200000223     C     FIL           SETGT     FNFVA03L
027300000223     C                   READ      FNFVA03L                               33
027400941215     C*
027500941215    4C     *IN33         IFEQ      *OFF
027600941215     C                   Z-ADD     FVALNP        FIL
027700941215     C*
027800941215     C     FIL           SETLL     FNFVA000
027900941215     C     FIL           READE     FNFVA000                               31
028000941215     C*
028100941215    5C     *IN31         DOWEQ     '0'
028200941215     C*
028300941215     C* SE DATA FOGLIO VIAGGIO > DATA PULIZIA LEGGO ALTRA LINEA
028400941215    6C     FVADFV        IFGT      COMFVA
028500941215     C                   SETON                                        31
028600941215   X6C                   ELSE
028700941215     C                   SETOFF                                       01
028800941215     C* CONTROLLO SE ESISTONO SPUNTE SU QUEL FOGLIO
028900941215     C                   EXSR      PULSPU
029000941215     C*
029100941215     C* SE TUTTO O.K. CANCELLO FV
029200000502     C  N36
029300000502     CANN01              DELETE    FNFWA000
029400941215     C  N01              DELETE    FNFVA000
029500941215     C*
029600941215     C     FIL           READE     FNFVA000                               31
029700941215    6C                   ENDIF
029800941215    5C                   ENDDO
029900941215     C*
030000941215    4C                   ENDIF
030100941215    3C                   ENDDO
030200021104     C****
030300021104     C** P U L I Z I A   F V 4
030400021104     C****
030500941215     C*
030600960214     C* CANCELLO FV4 SOLO SE NON ESISTE PIU' FNFVA
030700021104      * e FnFgv
030800960214     C     *LOVAL        SETLL     FNFV401L
030900960214     C                   READ      FNFV401L                             0136
031000960214    3C     *IN36         DOWEQ     *OFF
031100960214     C     *IN01         ANDEQ     *OFF
031200021104      * Per prima cosa devo controllare che non esista FGV
031300021104     c     Kfgv          setll     Fnfgv01l
031400021104If  4c                   If        Not %Equal(Fnfgv01l)
031500021104      * Poi controllo che non esista FVA
031600021104If  5c                   If        Fv4Lai > *Zeros
031700021104     c     Kfva          Setll     Fnfva01l
031800021104   x5c                   Else
031900021104     c     Kfva1         Setll     Fnfva01l
032000021104     c                   EndIf
032100021104If  5c                   If        Not %Equal(Fnfva01l)
032200960214     C* SE NON TROVATO CANCELLO
032300021104     C                   DELETE    FNFV4000
032400021104    5C                   ENDIF
032500021104    4C                   ENDIF
032600960314     C                   READ      FNFV401L                             0136
032700960214    3C                   ENDDO
032800960214     C*
032900960214    2C                   ENDIF
033000941215     C*
033100941215     C     KTAB          READE     TABEL00F                               30
033200941215    1C                   ENDDO
033300921111     C*
033400921111     C                   SETON                                        LR
033500941215     C*---------------------------------------------------------------*
033600941215     C*                    PULSPU                                     *
033700941215     C* ROUTINE DI CONTROLLO ESISTENZA SPUNTE PER QUEL FOGLIO         *
033800941215     C*---------------------------------------------------------------*
033900941215     C     PULSPU        BEGSR
034000941215     C*
034100000502     C     KFWA          CHAIN     FNFWA000                           36
034200000502     C   36              CLEAR                   FWAFF3
034300000502     C   36              CLEAR                   FWAFF4
034400000502     C   36              CLEAR                   FWAFL3
034500000502     C   36              CLEAR                   FWAFL4
034600000502     C*
034700000207     C                   Z-ADD     FVANFV        KNFVS
034800000502     C                   Z-ADD     FVALNP        KFGS
034900000502     C                   Z-ADD     FVALNA        LNA2
035000070118     C     KEYBRV        SETLL     FNBRV01L
035100070118     C     KEYBRV        READE     FNBRV01L                               35
035200941215     C*
035300941215    1C     *IN35         DOWEQ     *OFF
035400941215     C*
035500941215     C* UNA SPUNTA APPARTIENE A UN FOGLIO DI FNFVA:
035600941215     C*
035700941215     C*  - SE LINEA ARRIVO SPUNTA = LINEA INVIO FOGLIO
035800941215    2C     BRVLNA        IFEQ      FVALAI
035900941215     C                   SETON                                        3501
036000941215    2C                   ENDIF
036100941215     C*
036200941215     C*  - SE FIL.SCARICO DELLA LNA SPUNTA = LINEA INVIO FOGLIO
036300941215    2C     *IN35         IFEQ      *OFF
036400941215     C*
036500941215     C                   Z-ADD     1             X                 3 0
036600941215     C                   MOVEL     BRVLNA        WLNA              3
036700941215     C                   MOVEL     FVALAI        WLAI              3
036800941215     C     WLNA          LOOKUP    FFV(X)                                 20
036900941215    3C   20FLP(X)        IFEQ      WLAI
037000941215     C                   SETON                                        3501
037100941215    3C                   ENDIF
037200941215     C*
037300941215     C*  - SE TERMINAL ARRIVO DELLA LNA SPUNTA = LINEA INVIO FOGLIO
037400000502     C*    QUESTO 3° CASO PER LE SPUNTE NON ACCADE PERCHE' LE SPUNTE
037500000502     C*    INVIATE VENGONO IMMEDIATAENTE CANCELLATE DOPO L'AGGIORNAMENT
037600000502    2C                   ENDIF
037700941215     C*
037800070118     C  N35KEYBRV        READE     FNBRV01L                               35
037900941215    1C                   ENDDO
038000941215     C*
038100941215     C                   ENDSR
038200951207     C*---------------------------------------------------------------*
038300951207     C*                    FV2FV4                                     *
038400951207     C* ROUTINE DI PULIZIA FNFV200F E FNFV400F                        *
038500951207     C*---------------------------------------------------------------*
038600951207     C     FV2FV4        BEGSR
038700951207     C*
038800960214     C     KFVD          CHAIN     FNFV201L                           3601
038900960214    1C     *IN36         DOWEQ     *OFF
039000960214     C     *IN01         ANDEQ     *OFF
039100960214     C                   DELETE    FNFV2000
039200960214     C*
039300960214     C     KFVD          READE     FNFV201L                             0136
039400960214    1C                   ENDDO
039500951207     C*
039600021104    1C**!!!*IN01         IFEQ      *OFF
039700960214     C**
039800021104     C**!!!              SETOFF                                       37
039900021104     C**!!!KFVD          SETLL     FNFV401L
040000021104    2C**!!!*IN37         DOWEQ     *OFF
040100021104     C**!!!*IN01         ANDEQ     *OFF
040200021104     C**!!!KFVD          READE     FNFV401L                             0137
040300021104     C**!!!  N01
040400021104     C**!!!ANN37              DELETE    FNFV4000
040500021104    2C**!!!              ENDDO
040600021104    1C**!!!              ENDIF
040700951207     C*
040800951207     C                   ENDSR
040900921111     C*---------------------------------------------------------------*
041000921111     C*                    DEFVAR                                     *
041100921111     C*  ROUTINE DI DEFINIZIONE KEYLIST E VARIABILI                   *
041200921111     C*---------------------------------------------------------------*
041300921111     C     DEFVAR        BEGSR
041400921111     C*
041500930216     C     *ENTRY        PLIST
041600930216     C                   PARM                    KPJBA
041700930216     C                   MOVEL     KPJBU         PARAM
041800930216     C*
041900020809     ***C                   Z-ADD     1             CODUT
042000960214     C                   CALL      'X§PARUT'
042100020809     C                   PARM                    UT§DSE0F
042200960214     C                   MOVEL     REC80         CNCR80
042300930216     C*
042400921111     C* AGGANCIO FILE TABEL00F
042500941215     C     KTAB          KLIST
042600921111     C                   KFLD                    CODUT
042700921111     C                   KFLD                    COD
042800921111     C*
042900070118     C* AGGANCIO FILE FNBRV01L
043000921111     C     KEYBRV        KLIST
043100921111     C                   KFLD                    KNPG
043200000207     C                   KFLD                    KNFVS
043300941229     C                   KFLD                    KFGS
043400921111     C*
043500941215     C* AGGANCIO FILE FNFVD01L
043600930716     C     KFVD          KLIST
043700930716     C                   KFLD                    FGVLNP
043800930716     C                   KFLD                    FGVNFV
043900000502     C     KFWA          KLIST
044000000502     C                   KFLD                    FVALNP
044100000502     C                   KFLD                    FVANFV
044200000502     C                   KFLD                    FVALAI
044300020809     C*
044400020809     C* Aggancio file FNFGW01L
044500020809     C     K02FGW01      klist
044600020809     C                   kfld                    FGVnfv
044700020809     C                   kfld                    FGVlnp
044800950616     C*
044900070419     C     Kdst2         KLIST
045000070419     C                   KFLD                    fvvFGS
045100070419     C                   KFLD                    fvvnpg
045200070419     C                   KFLD                    wnfv
045300070419     C     Kdst3         KLIST
045400070419     C                   KFLD                    fvvFGS
045500070419     C                   KFLD                    wnfv
045600080725     C     Kdst4         KLIST
045700080725     C                   KFLD                    fvvnpg
045800080725     C                   KFLD                    wnfv
045900080725     C                   KFLD                    fvvFGS
046000930930     C*
046100021104      * Fnfgv01
046200021104     c     Kfgv          Klist
046300021104     c                   Kfld                    Fv4Nfv
046400021104     c                   Kfld                    Fv4Lnp
046500021104      * Fnfva01
046600021104     c     Kfva          Klist
046700021104     c                   Kfld                    Fv4Lnp
046800021104     c                   Kfld                    Fv4Nfv
046900021104     c                   Kfld                    Fv4Lai
047000021104     c     Kfva1         KLIST
047100021104     C                   KFLD                    FV4LNP
047200021104     C                   KFLD                    FV4NFV
047300020809     ***C*
047400020809     ***C     *LIKE         DEFINE    FVVFGS        KFGS
047500020809     ***C     *LIKE         DEFINE    FVVNPG        KNPG
047600020809     ***C     *LIKE         DEFINE    FVVNFV        KNFV
047700020809     ***C     *LIKE         DEFINE    TBLCOD        COD
047800020809     ***C     *LIKE         DEFINE    BRVNFV        KNFVS
047900930716     C*
048000921111     C                   ENDSR
