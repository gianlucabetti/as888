000100941214     H DECEDIT('0,') DATEDIT(*DMY.)
000200941214     F* FNLRA7R *-----------------------------------------------------*
000300941214     F*   PULIZIA F.VIAGGIO / FOGLI VARI / F.VIAGGIO PART. IN ARRIVO  *
000400921111     F*---------------------------------------------------------------*
000500941214     FFNFVV02L  UF   E           K DISK
000600941214     FFNFGV02L  UF   E           K DISK
000700000502     FFNFGW01L  UF   E           K DISK
000800130904     FFNFGS01L  UF   E           K DISK
000900000223     FFNFVA03L  UF   E           K DISK
001000000502     FFNFWA01L  UF   E           K DISK
001100000223     FFNFVA01L  IF   E           K DISK
001200000223     F                                     RENAME(FNFVA000:FNFVA001)
001300070118     FFNBRV01L  IF   E           K DISK
001400941214     FFNFVD01L  UF   E           K DISK
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
002700130904     fFnfge01l  uf   e           k Disk
002800130904     fFnfgb03l  uf   e           k Disk
002900141106     FFnque01l  UF   E           K DISK
003000941215     D*
003100941215     D* DEFINIZIONE SCHIERE
003200941215     D*
003300941215     D*LINEE   DI ARRIVO
003400941215     D                 DS
003500941215     D  FVALNA                 1      3  0
003600941215     D  FVAFFV                 4    243
003700941215     D  FVAFF2               244    450
003800000502     D  FWAFF3               451    690
003900000502     D  FWAFF4               691    900
004000000502     D  VUOTO                901    903
004100000502     D  FFV                    1    903
004200000502     D                                     DIM(301)                             LINEE FOGLIO
004300941215     D*FILIALI DI SCARICO
004400941215     D                 DS
004500941215     D  LNA2                   1      3  0
004600941215     D  FVAFLP                 4    243
004700941215     D  FVAFL2               244    450
004800000502     D  FWAFL3               451    690
004900000502     D  FWAFL4               691    900
005000000502     D  VUOTO2               901    903
005100000502     D  FLP                    1    903
005200000502     D                                     DIM(301)                             LINEE TRANSITO
005300921111     D*
005400020809     D KPJBA         E DS
005500020809     D UT§DSE0F      E DS
005600020809     D CNCR80        E DS
005700020809     D DS7N          E DS
005800921111     D*
005900921111     D PARAM           DS
006000941215     D* DATA PULIZIA F.VIAGGIO PARTENZA
006100941215     D  COMFGV                35     42  0
006200941215     D* DATA PULIZIA FOGLI VARI
006300941215     D  COMFVV                43     50  0
006400941215     D* DATA PULIZIA F.VIAGGIO PARTENZA IN ARRIVO
006500941215     D  COMFVA                51     58  0
006600130904     d* data pulizia etichette per buste documenti (FNFGE)
006700130904     D  comfge                60     67  0
006800130904     d* data pulizia richiesta bis (FNFGB)
006900130904     D  comfgb               247    254  0
007000020809      *
007100020809      * Variabili
007200020809      *
007300020809     D KFGS            S                   LIKE(FVVFGS)
007400020809     D KNPG            S                   LIKE(FVVNPG)
007500151021     D K2_NPG          S                   LIKE(FVVNPG)
007600020809     D KNFV            S                   LIKE(FVVNFV)
007700020809     D COD             S                   LIKE(TBLCOD)
007800020809     D KNFVS           S                   LIKE(BRVNFV)
007900050103     D Klnp            S                   LIKE(fgvlnp)
008000070419     d wnfv            s                   like(£dstnfv)
008100921111     C*---------------------------------------------------------------*
008200921111     C*  INDICATORI USATI                                             *
008300921111     C*---------------------------------------------------------------*
008400941215     C* 01    - DELETO LA SPUNTA
008500941215     C* 30/36 - DI COMODO
008600921111     C*---------------------------------------------------------------*
008700930216     C*
008800930216     C* DEFINIZIONE VARIABILI E KLIST
008900921111     C                   EXSR      DEFVAR
009000921111     C*
009100921112     C                   MOVEL     '7N'          COD
009200941215     C     KTAB          SETLL     TABEL00F
009300941215     C     KTAB          READE     TABEL00F                               30
009400930216     C*
009500941215    1C     *IN30         DOWEQ     *OFF
009600921111     C                   MOVEL     TBLUNI        DS7N
009700941215     C*
009800941214     C****
009900941214     C** P U L I Z I A   F O G L I   V A R I
010000941214     C****
010100941215    2C     §7NFFV        IFEQ      'S'
010200941214     C*
010300151021     C                   MOVEL     TBLKEY        KNPG
010400941214     C     KNPG          SETLL     FNFVV02L
010500941214     C     KNPG          READE     FNFVV02L                               31
010600921111     C*
010700941215    3C     *IN31         DOWEQ     *OFF
010800941215     C     FVVDFV        ANDLE     COMFVV
010900921111     C*
011000941229     C* FOGLIO CHIUSO = "S"
011100100705    4C**   FVVFCF        IFEQ      'S'
011200930930     C*
011300930930     C* OPPURE FOGLIO APERTO MA ANNULLATO
011400100705     C**   FVVFCF        ORNE      'S'
011500100705     C**   FVVATB        ANDNE     *BLANKS
011600930216     C*
011700930930     C* SE NON CI SONO SPUNTE ASSOCIATE CANCELLO
011800100705     c*  sempre
011900151021     C                   Z-ADD     FVVnpg        K2_NPG
012000151021     C                   Z-ADD     FVVNFV        KNFV
012100000207     C                   Z-ADD     FVVNFV        KNFVS
012200941229     C                   Z-ADD     FVVFGS        KFGS
012300151021     C     kbrv2         SETLL     FNBRV01L                               32
012400151021     c* Se foglio "4" controllo anche se presente spunta "8"
012500151021     c                   if        not *in32 and fvvnpg=4
012600151021     C                   movel     8             K2_NPG
012700151021     C     Kbrv2         SETLL     FNBRV01L                               32
012800151021     c                   endif
012900950616     C*
013000950616    5C     *IN32         IFEQ      *OFF
013100070419     C* CANCELLO RECORD IN FIDST00F SE ESISTE e estensione giri
013200070419     c                   eval      wnfv = fvvnfv
013300070419     c     kdst2         chain     fidst09l
013400070419     c                   if        %found(fidst09l) and
013500070419     c                             (£dstfcf = 'S' or (£dstfcf <> 'S' and
013600070419     c                             £dstatb <>' '))
013700070725     c*
013800070419     c     kdst3         setll     fidst31l
013900070419     c                   do        *hival
014000070419     c     kdst3         reade     fidst31l
014100070419     c                   if        %eof(fidst31l)
014200070419     c                   leave
014300070419     c                   end
014400070419     c                   delete    fidst300
014500070419     c                   enddo
014600070725     c*
014700070725     c     kdst2         setll     fidst41l
014800070725     c                   do        *hival
014900070725     c     kdst2         reade     fidst41l
015000070725     c                   if        %eof(fidst41l)
015100070725     c                   leave
015200070725     c                   end
015300070725     c                   delete    fidst400
015400070725     c                   enddo
015500070725     c*
015600070725     c     kdst3         setll     fidst51l
015700070725     c                   do        *hival
015800070725     c     kdst3         reade     fidst51l
015900070725     c                   if        %eof(fidst51l)
016000070725     c                   leave
016100070725     c                   end
016200070725     c                   delete    fidst500
016300070725     c                   enddo
016400070725     c*
016500070725     c     kdst2         setll     fidst61l
016600070725     c                   do        *hival
016700070725     c     kdst2         reade     fidst61l
016800070725     c                   if        %eof(fidst61l)
016900070725     c                   leave
017000070725     c                   end
017100070725     c                   delete    fidst600
017200070725     c                   enddo
017300080725     c*
017400080725     c     kdst4         setll     fidsf01l
017500080725     c                   do        *hival
017600080725     c     kdst4         reade     fidsf01l
017700080725     c                   if        %eof(fidsf01l)
017800080725     c                   leave
017900080725     c                   end
018000080725     c                   delete    fidsf000
018100080725     c                   enddo
018200080725     C                   DELETE    FiDST000
018300080725     c                   end
018400950616     C*
018500141106     c                   exsr      sr_que
018600141106
018700950616     C                   DELETE    FNFVV02L
018800100705     c
018900100705     c*
019000100705   x5c                   else
019100100705     c* se il foglio ha spunte ed è annullato, lo considero valido e lo
019200100705     c*  chiudo per permetterne la pulizia al momento giusto
019300100705    6c                   if        fvvatb<>' '
019400100705     c                   eval      fvvatb=' '
019500100705     c                   eval      fvvfcf='S'
019600100706     c                   eval      fvvdsf='Foglio Annullato chiuso da pulizie'
019700100705     c                   update    fnfvv000
019800100705    6c                   endif
019900100705     c*
020000100705     c* Se il foglio non è annnullato e aperto --> non chiudo le cat 4 e 8
020100100705     c                   if        fvvfcf=' ' and fvvnpg<>4 and fvvnpg<>8
020200100705     c                   eval      fvvdsf='CHIUSURA AUTOMATICA da pulizie'
020300100705     c                   eval      fvvfcf='S'
020400100705     c                   update    fnfvv000
020500100705    6c                   endif
020600141106     c                   exsr      sr_que
020700100705     c
020800950616    5C                   ENDIF
020900100705    4C*****              ENDIF
021000921111     C*
021100941214     C     KNPG          READE     FNFVV02L                               31
021200941215    3C                   ENDDO
021300921111     C*
021400941214     C*
021500941215    2C                   ELSE
021600941214     C****
021700941214     C** P U L I Z I A   F O G L I   V I A G G I O   P A R T E N Z E
021800941214     C****
021900960214     C                   Z-ADD     1             KNPG                           *CATEG.FOGLI
022000960214     C*
022100020809     C     *LOVAL        SETLL     FNFGV02L
022200020809     C                   READ      FNFGV02L                               33
022300921111     C*
022400941215    3C     *IN33         DOWEQ     *OFF
022500050103     c
022600050103     c* Elaboro tutte le linee di partenza
022700050103     c                   z-add     fgvlnp        klnp
022800050103     C     klnp          SETLL     FNFGV02L
022900050103     C     klnp          READe     FNFGV02L                               33
023000050103     c*
023100050103   3aC     *IN33         DOWEQ     *OFF
023200941215     C     FGVDFV        ANDLE     COMFGV
023300921111     C*
023400921111     C* FOGLIO CHIUSO
023500941215    4C     FGVFCF        IFNE      *BLANKS
023600000502     C     FGVATB        ORNE      ' '
023700930216     C*
023800930930     C* CONTROLLO SE NON CI SONO SPUNTE ASSOCIATE
023900000207     C                   Z-ADD     FGVNFV        KNFVS
024000941229     C                   Z-ADD     FGVLNP        KFGS
024100100705     C     KEYBRV        SETLL     FNBRV01L                               34
024200930216     C*
024300960214    5C     *IN34         IFEQ      *OFF
024400930216     C* E SE NON CI SONO BOLLE: CANCELLO
024500020809     ***C     FGVNFV        SETLL     FNBLP29L                               35
024600020809     C     KFVD          setll     FNBLP79L                               35
024700930716     C*
024800941215    6C     *IN35         IFEQ      *OFF
024900930928     C*
025000941214     C* SE ESISTONO RECORD IN FNFVD LI DELETO
025100941214     C     KFVD          SETLL     FNFVD000
025200000502     C                   SETOFF                                       3601
025300941215    7C     *IN36         DOWEQ     *OFF
025400000502     C     *IN01         ANDEQ     *OFF
025500000502     C     KFVD          READE     FNFVD000                             0136
025600000502     C  N01
025700000502     CANN36              DELETE    FNFVD000
025800941215    7C                   ENDDO
025900930716     C*
026000951207     C* SE ESISTONO RECORD IN FNFV2/FNFV4 LI DELETO
026100021104      * da 04/11/02 solo per fv2
026200000502     C  N01              EXSR      FV2FV4
026300130904     C* SE ESISTe FNFGS Lo DELETO
026400130904     C  N01K02FGW01      delete    FNFGs000                           3301
026500000502     C* SE ESISTE FNGW LO DELETO
026600020809     ***C  N01FGVNFV        DELETE    FNFGW000                           3301
026700020809     C  N01K02FGW01      delete    FNFGW000                           3301
026800951207     C*
026900941214     C  N01              DELETE    FNFGV02L
027000941215    6C                   ENDIF
027100930716     C*
027200941215    5C                   ENDIF
027300020809     C*
027400941215    4C                   ENDIF
027500921112     C*
027600050103     C     klnp          READE     FNFGV02L                               33
027700050103   3AC                   ENDDO
027800050103     C*
027900050103     C     klnp          SETGT     FNFGV02L
028000050103     C                   READ      FNFGV02L                               33
028100050103    3C                   ENDDO
028200050103     C
028300921111     C*
028400941214     C****
028500941214     C** P U L I Z I A   F O G L I   V I A G G I O   P A R T E N Z E
028600941214     C** I N   A R R I V O
028700941214     C****
028800941215     C                   Z-ADD     0             FIL               3 0
028900941215     C                   SETOFF                                       33
029000941215     C*
029100941215    3C     *IN33         DOWEQ     *OFF
029200000223     C     FIL           SETGT     FNFVA03L
029300000223     C                   READ      FNFVA03L                               33
029400941215     C*
029500941215    4C     *IN33         IFEQ      *OFF
029600941215     C                   Z-ADD     FVALNP        FIL
029700941215     C*
029800941215     C     FIL           SETLL     FNFVA000
029900941215     C     FIL           READE     FNFVA000                               31
030000941215     C*
030100941215    5C     *IN31         DOWEQ     '0'
030200941215     C*
030300941215     C* SE DATA FOGLIO VIAGGIO > DATA PULIZIA LEGGO ALTRA LINEA
030400941215    6C     FVADFV        IFGT      COMFVA
030500941215     C                   SETON                                        31
030600941215   X6C                   ELSE
030700941215     C                   SETOFF                                       01
030800941215     C* CONTROLLO SE ESISTONO SPUNTE SU QUEL FOGLIO
030900941215     C                   EXSR      PULSPU
031000941215     C*
031100941215     C* SE TUTTO O.K. CANCELLO FV
031200000502     C  N36
031300000502     CANN01              DELETE    FNFWA000
031400941215     C  N01              DELETE    FNFVA000
031500941215     C*
031600941215     C     FIL           READE     FNFVA000                               31
031700941215    6C                   ENDIF
031800941215    5C                   ENDDO
031900941215     C*
032000941215    4C                   ENDIF
032100941215    3C                   ENDDO
032200021104     C****
032300021104     C** P U L I Z I A   F V 4
032400021104     C****
032500941215     C*
032600960214     C* CANCELLO FV4 SOLO SE NON ESISTE PIU' FNFVA
032700021104      * e FnFgv
032800960214     C     *LOVAL        SETLL     FNFV401L
032900960214     C                   READ      FNFV401L                             0136
033000960214    3C     *IN36         DOWEQ     *OFF
033100960214     C     *IN01         ANDEQ     *OFF
033200021104      * Per prima cosa devo controllare che non esista FGV
033300021104     c     Kfgv          setll     Fnfgv01l
033400021104If  4c                   If        Not %Equal(Fnfgv01l)
033500021104      * Poi controllo che non esista FVA
033600021104If  5c                   If        Fv4Lai > *Zeros
033700021104     c     Kfva          Setll     Fnfva01l
033800021104   x5c                   Else
033900021104     c     Kfva1         Setll     Fnfva01l
034000021104     c                   EndIf
034100021104If  5c                   If        Not %Equal(Fnfva01l)
034200960214     C* SE NON TROVATO CANCELLO
034300021104     C                   DELETE    FNFV4000
034400021104    5C                   ENDIF
034500021104    4C                   ENDIF
034600960314     C                   READ      FNFV401L                             0136
034700960214    3C                   ENDDO
034800960214     C*
034900960214    2C                   ENDIF
035000941215     C*
035100941215     C     KTAB          READE     TABEL00F                               30
035200941215    1C                   ENDDO
035300130904     c* Pulizia file per stampa etichette FNFGE
035400130904     c     *loval        setll     fnfge01l
035500130904     c                   read      fnfge01l
035600130904     c                   dow       not %eof(fnfge01l) and
035700130904     c                             fgedfv<=comfge
035800130904     c                   delete    fnfge000
035900130904     c                   read      fnfge01l
036000130904     c                   enddo
036100130904     c* Pulizia file per richiesta Bis    FNFGB
036200130904     c     *loval        setll     fnfgb03l
036300130904     c                   read      fnfgb03l
036400130904     c                   dow       not %eof(fnfgb03l) and
036500130904     c                             fgbdtn<=comfgb
036600130904     c                   delete    fnfgb000
036700130904     c                   read      fnfgb03l
036800130904     c                   enddo
036900921111     C*
037000921111     C                   SETON                                        LR
037100941215     C*---------------------------------------------------------------*
037200941215     C*                    PULSPU                                     *
037300941215     C* ROUTINE DI CONTROLLO ESISTENZA SPUNTE PER QUEL FOGLIO         *
037400941215     C*---------------------------------------------------------------*
037500941215     C     PULSPU        BEGSR
037600941215     C*
037700000502     C     KFWA          CHAIN     FNFWA000                           36
037800000502     C   36              CLEAR                   FWAFF3
037900000502     C   36              CLEAR                   FWAFF4
038000000502     C   36              CLEAR                   FWAFL3
038100000502     C   36              CLEAR                   FWAFL4
038200000502     C*
038300000207     C                   Z-ADD     FVANFV        KNFVS
038400000502     C                   Z-ADD     FVALNP        KFGS
038500000502     C                   Z-ADD     FVALNA        LNA2
038600070118     C     KEYBRV        SETLL     FNBRV01L
038700070118     C     KEYBRV        READE     FNBRV01L                               35
038800941215     C*
038900941215    1C     *IN35         DOWEQ     *OFF
039000941215     C*
039100941215     C* UNA SPUNTA APPARTIENE A UN FOGLIO DI FNFVA:
039200941215     C*
039300941215     C*  - SE LINEA ARRIVO SPUNTA = LINEA INVIO FOGLIO
039400941215    2C     BRVLNA        IFEQ      FVALAI
039500941215     C                   SETON                                        3501
039600941215    2C                   ENDIF
039700941215     C*
039800941215     C*  - SE FIL.SCARICO DELLA LNA SPUNTA = LINEA INVIO FOGLIO
039900941215    2C     *IN35         IFEQ      *OFF
040000941215     C*
040100941215     C                   Z-ADD     1             X                 3 0
040200941215     C                   MOVEL     BRVLNA        WLNA              3
040300941215     C                   MOVEL     FVALAI        WLAI              3
040400941215     C     WLNA          LOOKUP    FFV(X)                                 20
040500941215    3C   20FLP(X)        IFEQ      WLAI
040600941215     C                   SETON                                        3501
040700941215    3C                   ENDIF
040800941215     C*
040900941215     C*  - SE TERMINAL ARRIVO DELLA LNA SPUNTA = LINEA INVIO FOGLIO
041000000502     C*    QUESTO 3° CASO PER LE SPUNTE NON ACCADE PERCHE' LE SPUNTE
041100000502     C*    INVIATE VENGONO IMMEDIATAENTE CANCELLATE DOPO L'AGGIORNAMENT
041200000502    2C                   ENDIF
041300941215     C*
041400070118     C  N35KEYBRV        READE     FNBRV01L                               35
041500941215    1C                   ENDDO
041600941215     C*
041700941215     C                   ENDSR
041800951207     C*---------------------------------------------------------------*
041900951207     C*                    FV2FV4                                     *
042000951207     C* ROUTINE DI PULIZIA FNFV200F E FNFV400F                        *
042100951207     C*---------------------------------------------------------------*
042200951207     C     FV2FV4        BEGSR
042300951207     C*
042400960214     C     KFVD          CHAIN     FNFV201L                           3601
042500960214    1C     *IN36         DOWEQ     *OFF
042600960214     C     *IN01         ANDEQ     *OFF
042700960214     C                   DELETE    FNFV2000
042800960214     C*
042900960214     C     KFVD          READE     FNFV201L                             0136
043000960214    1C                   ENDDO
043100951207     C*
043200021104    1C**!!!*IN01         IFEQ      *OFF
043300960214     C**
043400021104     C**!!!              SETOFF                                       37
043500021104     C**!!!KFVD          SETLL     FNFV401L
043600021104    2C**!!!*IN37         DOWEQ     *OFF
043700021104     C**!!!*IN01         ANDEQ     *OFF
043800021104     C**!!!KFVD          READE     FNFV401L                             0137
043900021104     C**!!!  N01
044000021104     C**!!!ANN37              DELETE    FNFV4000
044100021104    2C**!!!              ENDDO
044200021104    1C**!!!              ENDIF
044300951207     C*
044400951207     C                   ENDSR
044500141106     C*---------------------------------------------------------------*
044600141106     C* ROUTINE DI PULIZIA FNQUE00F                                   *
044700141106     C*---------------------------------------------------------------*
044800141106     C     sr_que        BEGSR
044900141106     c                   eval      wnfv = fvvnfv
045000141106     c* cancello RECORD in FNQUE se presenti
045100141106     c     kque          setll     fnque01l
045200141106     c                   do        *hival
045300141106     c     kque          reade     fnque01l
045400141106     c                   if        %eof(fnque01l)
045500141106     c                   leave
045600141106     c                   end
045700141106     c                   delete    fnque000
045800141106     c                   enddo
045900141106     c                   endsr
046000921111     C*---------------------------------------------------------------*
046100921111     C*                    DEFVAR                                     *
046200921111     C*  ROUTINE DI DEFINIZIONE KEYLIST E VARIABILI                   *
046300921111     C*---------------------------------------------------------------*
046400921111     C     DEFVAR        BEGSR
046500921111     C*
046600930216     C     *ENTRY        PLIST
046700930216     C                   PARM                    KPJBA
046800930216     C                   MOVEL     KPJBU         PARAM
046900930216     C*
047000020809     ***C                   Z-ADD     1             CODUT
047100960214     C                   CALL      'X§PARUT'
047200020809     C                   PARM                    UT§DSE0F
047300960214     C                   MOVEL     REC80         CNCR80
047400930216     C*
047500921111     C* AGGANCIO FILE TABEL00F
047600941215     C     KTAB          KLIST
047700921111     C                   KFLD                    CODUT
047800921111     C                   KFLD                    COD
047900921111     C*
048000070118     C* AGGANCIO FILE FNBRV01L
048100921111     C     KEYBRV        KLIST
048200921111     C                   KFLD                    KNPG
048300000207     C                   KFLD                    KNFVS
048400941229     C                   KFLD                    KFGS
048500151021     C     Kbrv2         KLIST
048600151021     C                   KFLD                    K2_NPG
048700151021     C                   KFLD                    KNFVS
048800151021     C                   KFLD                    KFGS
048900921111     C*
049000941215     C* AGGANCIO FILE FNFVD01L
049100930716     C     KFVD          KLIST
049200930716     C                   KFLD                    FGVLNP
049300930716     C                   KFLD                    FGVNFV
049400000502     C     KFWA          KLIST
049500000502     C                   KFLD                    FVALNP
049600000502     C                   KFLD                    FVANFV
049700000502     C                   KFLD                    FVALAI
049800020809     C*
049900020809     C* Aggancio file FNFGW01L
050000020809     C     K02FGW01      klist
050100020809     C                   kfld                    FGVnfv
050200020809     C                   kfld                    FGVlnp
050300950616     C*
050400070419     C     Kdst2         KLIST
050500070419     C                   KFLD                    fvvFGS
050600070419     C                   KFLD                    fvvnpg
050700070419     C                   KFLD                    wnfv
050800070419     C     Kdst3         KLIST
050900070419     C                   KFLD                    fvvFGS
051000070419     C                   KFLD                    wnfv
051100080725     C     Kdst4         KLIST
051200080725     C                   KFLD                    fvvnpg
051300080725     C                   KFLD                    wnfv
051400080725     C                   KFLD                    fvvFGS
051500930930     C*
051600141106     C     Kque          KLIST
051700141106     C                   KFLD                    fvvnpg
051800141106     C                   KFLD                    wnfv
051900141106     C                   KFLD                    fvvFGS
052000021104      * Fnfgv01
052100021104     c     Kfgv          Klist
052200021104     c                   Kfld                    Fv4Nfv
052300021104     c                   Kfld                    Fv4Lnp
052400021104      * Fnfva01
052500021104     c     Kfva          Klist
052600021104     c                   Kfld                    Fv4Lnp
052700021104     c                   Kfld                    Fv4Nfv
052800021104     c                   Kfld                    Fv4Lai
052900021104     c     Kfva1         KLIST
053000021104     C                   KFLD                    FV4LNP
053100021104     C                   KFLD                    FV4NFV
053200020809     ***C*
053300020809     ***C     *LIKE         DEFINE    FVVFGS        KFGS
053400020809     ***C     *LIKE         DEFINE    FVVNPG        KNPG
053500020809     ***C     *LIKE         DEFINE    FVVNFV        KNFV
053600020809     ***C     *LIKE         DEFINE    TBLCOD        COD
053700020809     ***C     *LIKE         DEFINE    BRVNFV        KNFVS
053800930716     C*
053900921111     C                   ENDSR
