000100031219     H DECEDIT('0,') DATEDIT(*ymd.)
000200970424     F* FNLV53R *-----------------------------------------------------*
000300970424     F*       REPERIMENTO DATI FOGLIO DA SPUNTE
000400950130     F*---------------------------------------------------------------*
000500970602     FTABEL00F  IF   E           K DISK    USROPN
000600000609     FAZORG01L  IF   E           K DISK
000700970424     FFNFVV01L  IF   E           K DISK
000800950130     F*
000900970424     FFNFGV01L  IF   E           K DISK
001000040701     FFNFGw02L  IF   E           K DISK
001100040701     F                                     RENAME(FNFgw000:FNFgW002)
001200970424     FFNFVA01L  IF   E           K DISK
001300000427     FFNFWA01L  IF   E           K DISK
001400040512     FFNFgw01L  IF   E           K DISK
001500000609     FFNFWA02L  IF   E           K DISK
001600000609     F                                     RENAME(FNFWA000:FNFWA002)
001700040512     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001800970502     D TTR             S              1    DIM(10)
001900970424     D CNCR80        E DS
002000970424     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002100970502     D DSTV          E DS
002200970424     D* DS PER DSLV53R - PASSAGGIO PARAMETRI
002300970424     D DSLV53        E DS                  EXTNAME(FNLV53DS)
002400970515     D* DS PER DSLV55R - PASSAGGIO PARAMETRI
002500970515     D DSLV55        E DS                  EXTNAME(FNLV55DS)
002600000609     D OG143         E DS
002700970430     D                 DS
002800000427     D  FFV                    1    903
002900000427     D                                     DIM(301)
003000970430     D  FVALNA                 1      3  0
003100970430     D  FVAFFV                 4    243
003200000525     D  FVAFF2               244    450
003300000427     D  FWAFF3               451    690
003400000427     D  FWAFF4               691    900
003500000427     D  VUOTO                901    903
003600970430     D*
003700970430     D                 DS
003800000427     D  FLP                    1    903
003900000427     D                                     DIM(301)
004000970430     D  LNA2                   1      3
004100970430     D  FVAFLP                 4    243
004200970430     D  FVAFL2               244    450
004300000427     D  FWAFL3               451    690
004400000427     D  FWAFL4               691    900
004500000427     D  VUOTO2               901    903
004600040511     d
004700040511     d*
004800040511     D  FFVG                   1    903
004900040511     D                                     DIM(301)
005000040511     D  FgvLNA                 1      3  0
005100040511     D  FgvFFV                 4    243
005200040511     D  FgvFF2               244    450
005300040511     D  FgwFF3               451    690
005400040511     D  FgwFF4               691    900
005500040511     D  VUOTOg               901    903
005600040511     D*
005700040511     D                 DS
005800040511     D  FLPg                   1    903
005900040511     D                                     DIM(301)
006000040511     D  LNA2g                  1      3
006100040511     D  FgvFLP                 4    243
006200040511     D  FgvFL2               244    450
006300040511     D  FgwFL3               451    690
006400040511     D  FgwFL4               691    900
006500040511     D  VUOTO2g              901    903
006600020923      *
006700040701     D*wD53FGS         s                   like(D53fgs) inz
006800031211     D WTROV           s              1
006900970424     C**************************************************************************
007000970424     C     *ENTRY        PLIST
007100970424     C                   PARM                    DSLV53
007200970424     C*
007300970424     C* TIPO LANCIO, D53TLA "C" -           CHIUSO CON LR
007400970424     C* TIPO LANCIO, D53TLA "L" - ELABORO E CHIUDO CON LR
007500970424     C* TIPO LANCIO, D53TLA " " - ELABORO E CHIUDO IN RETRN
007600970424    1C     D53TLA        IFNE      'C'
007700970424     C                   CLEAR                   D53ERR
007800970424     C                   CLEAR                   D53MSG
007900031204     C
008000031204     C* SE NON HO P.O. GESTIONE --> NON POSSO CERCARE IL FOGLIO
008100031204     C                   IF        D53FGS=0
008200031204     C                   MOVEL     MSG(7)        D53MSG
008300031204     C                   MOVEL     '7'           D53ERR
008400031204     C                   GOTO      FINE
008500031204    3C                   ENDIF
008600031204     C* SE MANCA IL NUMERO FOGLIO ERRORE
008700031204    1C     D53NFV        IFEQ      0
008800031204     C                   MOVEL     '4'           D53ERR
008900031204     C                   MOVEL     MSG(4)        D53MSG
009000031204     C                   GOTO      FINE
009100031204    3C                   ENDIF
009200031204     C* SE tipo J e richiesti i dati di abbinamento errore
009300031204    1C     D53TFO        IFEQ      'J'
009400031204     c     D53ABB        andne     *blanks
009500031204     C                   MOVEL     '9'           D53ERR
009600031204     C                   MOVEL     MSG(9)        D53MSG
009700031204     C                   GOTO      FINE
009800031204    3C                   ENDIF
009900031219      *
010000031219      *  Controllo P.O. Gestione passato come parametro
010100031219      *  (se POSTE: come chiave per FNFGV01L devo usare SIMFEL)
010200031219    0c                   if        d53npg=1 or d53tfo<>'V'
010300040701     c***                eval      wD53fgs = D53fgs
010400031219    1C     D53FGS        IFNE      SA2FGS
010500031219     c                   clear                   OG143
010600031219     c     D53fgs        chain     AZORG
010700031219    2c                   if        %found(AZORG01L)
010800031219     c                   movel     ORGDE3        OG143
010900031219    2c                   endif
011000031219     C                   MOVEL     D53FGS        SA2FGS
011100031219    1C                   ENDIF
011200040701    cc
011300040701    1c**                 if        §OGntw  = 'PPT'
011400031219     c* sae poste e manca d53flf --> lo determino
011500040701    2c**                 if        d53flf>0
011600040701     c**                 eval      wD53fgs = D53flf
011700040701   x2c**                 else
011800031219     c* richiamo fnlv55r
011900040701     C**                 CLEAR                   DSLV55
012000040701     C**                 MOVE      D53fgs        D55LIN
012100040701     C**                 Z-ADD     *date         D55DRF
012200040701     C**                 MOVEL     'P'           D55TPT
012300031219     C*
012400040701     C**                 CALL      'FNLV55R'
012500040701     C**                 PARM                    DSLV55
012600040701     c**                 eval      wD53fgs = D55tfp
012700040701    2c**                 endif
012800040701    1c**                 endif
012900031219    0c                   endif
013000970424     C*
013100970424     C** TIPO FOGLIO NON PASSATO --> LO DESUMO
013200970424    2C     D53TFO        IFEQ      ' '
013300970424     C**
013400970424     C* ERRORE SE NON CI SONO TUTTI I DATI NECESSARI
013500970424    3C     D53NPG        IFEQ      0
013600031219     C**   D53FLF        OREQ      0
013700970424     C                   MOVEL     MSG(1)        D53MSG
013800970424     C                   MOVEL     '1'           D53ERR
013900970424     C                   GOTO      FINE
014000970424    3C                   ENDIF
014100970424     C*
014200970424    3C     D53NPG        IFEQ      1
014300031219     C                   MOVEL     'J'           D53TFO
014400970424     C*
014500031219    4C**   D53FLF        IFEQ      D53FEL
014600031219     C**                 MOVEL     'G'           D53TFO
014700031219     C**                 ELSE
014800031219     C**                 MOVEL     'A'           D53TFO
014900031219    4C**                 ENDIF
015000970424     C**
015100970424   X3C                   ELSE
015200970424     C                   MOVEL     'V'           D53TFO
015300970424    3C                   ENDIF
015400970424    2C                   ENDIF
015500970424     C**
015600970424     C                   EXSR      RILFOG
015700970424     C*
015800970424     C     FINE          TAG
015900970424    1C                   ENDIF
016000970424     C* CHIUSURA PGM
016100970424    1C     D53TLA        IFEQ      ' '
016200970424     C                   RETURN
016300970424     C                   ELSE
016400970515     C     WLV55         IFEQ      '1'
016500970515     C                   MOVEL     'C'           D55TLA
016600970515     C                   CALL      'FNLV55R'
016700970515     C                   PARM                    DSLV55
016800970515     C                   END
016900970424     C                   SETON                                        LR
017000970424    1C                   ENDIF
017100970424     C**************************************************************************
017200970424     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI
017300970424     C**************************************************************************
017400970424     C     RILFOG        BEGSR
017500970424     C                   CLEAR                   D53DFV
017600970424     C                   CLEAR                   D53SPG
017700970424     C                   CLEAR                   D53FCF
017800970424     C                   CLEAR                   D53ATB
017900970424     C                   CLEAR                   D53TTR
018000970502     C                   CLEAR                   D53DEF
018100970424     C                   CLEAR                   D53NFA
018200970424     C                   CLEAR                   D53DFA
018300970424     C                   CLEAR                   D53NPA
018400970430     C                   CLEAR                   D53AED
018500970430     C                   CLEAR                   D53AEH
018600970424     C**
018700970424     C* F O G L I O   V I A G G I O   P A R T E N Z A
018800970424     C**
018900031204    1C                   SELECT
019000031204    1C     D53TFO        WHENEQ    'G'
019100031204     c                   EXSR      LEGFGV
019200031204     C* Non trovato foglio
019300031204   X2C                   IF        *IN99
019400970424     C                   MOVEL     '2'           D53ERR
019500970424     C                   MOVEL     MSG(2)        D53MSG
019600040512     c                   else
019700040512     c                   if        WFVAOK='N'
019800040512     C                   MOVEL     '6'           D53ERR
019900040512     C                   MOVEL     MSG(10)       D53MSG
020000031204    2C                   ENDIF
020100040512    2C                   ENDIF
020200970424     C**
020300970424     C* F O G L I O   V I A G G I O   P A R T E N Z A
020400970424     C*               I N   A R R I V O
020500970424     C**
020600031204    1C     D53TFO        WHENEQ    'A'
020700031204     C     D53TFO        OREQ      'T'
020800031204     c                   EXSR      LEGFVA
020900031204     c* Foglio non trovato
021000040112   X2C**                 if        *in99
021100040112     c                   if        WFVAOK='X'
021200970424     C                   MOVEL     '2'           D53ERR
021300970424     C                   MOVEL     MSG(2)        D53MSG
021400031204    2C                   ENDIF
021500031204     c
021600040112    2C**   WFVAOK        IFne      *BLANKS
021700040112     c                   if        WFVAOK='N'
021800031204     C                   MOVEL     '6'           D53ERR
021900031204     C                   MOVEL     MSG(6)        D53MSG
022000031204    2C                   END
022100031204     c**
022200031204     C** D O P P I O   T E N T A T I V O   N E I   F O G L I
022300031204     C**               P A R T E N Z A
022400031204    1C     D53TFO        WHENEQ    'J'
022500031204     C* SE non richiesto l'abbinamento cerco prima in FGV, altrimenti
022600031204     C*  in FVA
022700031204     c     D53ABB        andeq     *blanks
022800031204     c                   EXSR      LEGFGV
022900031204     C* Non trovato foglio
023000031204   X2C                   IF        *IN99
023100031204     c                   EXSR      LEGFVA
023200031204     c* Foglio non trovato
023300040112   X3C**                 if        *in99
023400040112     c                   if        WFVAOK='X'
023500031204     C                   MOVEL     '8'           D53ERR
023600031204     C                   MOVEL     MSG(8)        D53MSG
023700031204    3C                   ENDIF
023800031204    2C                   END
023900031204     c**
024000031204     c**
024100031204    1C**   D53TFO        WHENEQ    'J'
024200031204     C* SE richiesto l'abbinamento cerco prima in FVA, altrimenti
024300031204     C*  in FGV
024400031204     c**   D53ABB        andne     *blanks
024500031204     C* Non trovato foglio
024600031204     c**                 EXSR      LEGFVA
024700031204     c*
024800031204    2C**                 if        not *in99
024900031204    3C**   WFVAOK        IFne      *BLANKS
025000031204     C**                 MOVEL     '6'           D53ERR
025100031204     C**                 MOVEL     MSG(6)        D53MSG
025200031204    3C**                 END
025300031204     c
025400031204   x2c**                 else
025500031204     c
025600031204     c**                 EXSR      LEGFGV
025700031204   X3C**                 if        *in99
025800031204     C**                 MOVEL     '8'           D53ERR
025900031204     C**                 MOVEL     MSG(8)        D53MSG
026000031204    3C**                 ENDIF
026100031204    2C**                 ENDIF
026200031204     c
026300970424     C* F O G L I O   V A R I O
026400970424     C**
026500970424    2C     D53TFO        WHENEQ    'V'
026600970424     C* CI DEVE ESSERE CATEGORIA E P.O. GESTIONE O CATEGORIA E FLE
026700970424    3C     D53FGS        IFGT      0
026800970424     C     D53NPG        ANDGT     0
026900970424     C*
027000020919     C**   D53FLF        ORGT      0
027100020919     C**   D53NPG        ANDGT     0
027200970424     C* SE C'E' LA FILIALE GESTIONE USO QUELLA
027300970424     C* ALTRIMENTI USO LA FILIALE ELABORATORE FOGLIO
027400020917      * Ora riceve sempre il p.o. in gestione
027500970424     C     KFVV1         CHAIN     FNFVV01L                           99
027600970424     C**
027700970424    4C     *IN99         IFEQ      *OFF
027800970424     C                   Z-ADD     FVVFGS        D53FGS
027900970424     C                   Z-ADD     FVVFLE        D53FLF
028000970424     C                   Z-ADD     FVVDFV        D53DFV
028100970424     C                   MOVEL     FVVFCF        D53FCF
028200970424     C                   MOVEL     FVVATB        D53ATB
028300970502     C                   MOVEL     FVVSPG        D53SPG
028400970424   X4C                   ELSE
028500970424     C                   MOVEL     '3'           D53ERR
028600970424     C                   MOVEL     MSG(3)        D53MSG
028700970424    4C                   ENDIF
028800970424     C** ERRORE
028900970424   X3C                   ELSE
029000970424     C                   MOVEL     '3'           D53ERR
029100970424     C                   MOVEL     MSG(3)        D53MSG
029200970424    3C                   ENDIF
029300970424    2C                   ENDSL
029400970424     C*
029500970424     C                   ENDSR
029600970430     C**************************************************************************
029700970430     C* CONTROLLO SE IL FV E' ABBINATO
029800970430     C**************************************************************************
029900031211     C     CTRFVA        BEGSR
030000970430     C*
030100970430     C                   CLEAR                   WTROV
030200970602     C* IMPOSTO WTFA=0      SE DEVO DETERMINARE IL TER-ARR
030300970602     C* IMPOSTO WTFA=D53TFA SE IL TER-ARR NON E' DA DETERMINARE
030400970602     C     D53TFA        IFEQ      *ZEROS
030500970602     C     D53FGS        IFNE      SAVFGS
030600970602     C     D53LNA        ORNE      SAVLNA
030700970602     C     FVADFV        ORNE      SAVDFV
030800970602     C                   CLEAR                   WTFA
030900970602     C                   END
031000970602     C                   ELSE
031100970602     C                   MOVE      D53TFA        WTFA
031200970602     C                   END
031300970602     C*
031400970602     C                   MOVE      D53FGS        SAVFGS
031500970602     C                   MOVE      D53LNA        SAVLNA
031600970602     C                   MOVE      FVADFV        SAVDFV
031700970430     C*
031800031211    1C**   D53LNA        IFEQ      FVALAI
031900031211     C**   D53TFO        ANDEQ     'A'
032000031211     C**                 MOVEL     'S'           WTROV             1
032100031211   X1C**                 ELSE
032200970430     C*
032300020122     C* ESTENSIONE FOGLI PER REPERIRE TUTTE LE LINEE
032400020917     c                   If        §ogntw <> 'PPT'
032500020122     C     KFWA          CHAIN     FNFWA01L                           33
032600020122     C   33              CLEAR                   FWAFF3
032700020122     C   33              CLEAR                   FWAFF4
032800020122     C   33              CLEAR                   FWAFL3
032900020122     C   33              CLEAR                   FWAFL4
033000020122     C                   ENDIF
033100020618     C*
033200020618     C* A CAMBIO LNA O WTFA DETERMINO I TERMINAL DI PART. E ARRIVO
033300031211    1C     D53LNA        IFNE      LNASAV
033400020618     C     WTFA          OREQ      *ZEROS
033500020618     C                   MOVE      D53LNA        LNASAV
033600020618     C                   CLEAR                   DSLV55
033700020618     C                   MOVE      D53LNA        D55LIN
033800020618     C                   Z-ADD     FVADFV        D55DRF
033900031211    2C     WTFA          IFEQ      *ZEROS
034000020618     C                   MOVE      FVALNP        D55LNP
034100031211   X2C                   ELSE
034200020618     C                   MOVEL     'P'           D55TPT
034300031211    2C                   END
034400020618     C*
034500020618     C                   CALL      'FNLV55R'
034600020618     C                   PARM                    DSLV55
034700020618     C                   MOVE      '1'           WLV55             1
034800031211    2C     D55TPT        IFNE      'P'
034900020618     C                   MOVE      D55TFA        WTFA
035000031211    2C                   END
035100031211     c                   eval      lnatfp=D55tfp
035200031211    1C                   END
035300031211     c*
035400031211     C*  CERCO CON LA SUA FILIALE DI SCARICO
035500970430     C                   MOVEL     D53LNA        W003A             3
035600970430     C                   MOVEL     FVALNA        LNA2
035700970430     C                   Z-ADD     1             YY                3 0
035800970430     C     W003A         LOOKUP    FFV(YY)                                33
035900031211     C  N33              CLEAR                   WFLP
036000031211     C   33              MOVEL     FLP(YY)       WFLP
036100031211     c*
036200031211     c* Determino terminal partenza del p.o. scarico
036300031211     C                   CLEAR                   DSLV55
036400031211     C                   MOVE      WFLP          D55LIN
036500031211     C                   Z-ADD     FVADFV        D55DRF
036600031211     C                   MOVEL     'P'           D55TPT
036700031211     C*
036800031211     C                   CALL      'FNLV55R'
036900031211     C                   PARM                    DSLV55
037000031211     C                   MOVE      '1'           WLV55             1
037100031211     c                   eval      flptfp=D55tfp
037200970430     C*
037300031211    1C     *IN99         DOWEQ     *OFF
037400970430     C     WTROV         ANDEQ     *BLANKS
037500970430     C*
037600031211    2C**   D53LNA        IFEQ      FVALAI
037700031211     C**   D53TFO        ANDEQ     'A'
037800031211     C**                 MOVEL     'S'           WTROV
037900031211    2C**                 ENDIF
038000970430     C*
038100031211     c* Per i transiti considero solo p.o. scarico = linea invio foglio
038200031211    2C     WFLP          IFEQ      FVALAI
038300020617     C     D53TFO        ANDEQ     'T'
038400031211     c*
038500031211     c* Per le bolle in arrivo considero p.o. scar = linea invio foglio
038600031211     c*  se è il terminal di arrivo
038700031211    2C     WFLP          OREQ      FVALAI
038800031211     C     WFLP          ANDEQ     WTFA
038900020617     C     D53TFO        ANDEQ     'A'
039000051020     c
039100051020     c* Aggiungo un caso: i fogli diretti che non rientravano in nessuna
039200051020     c*  casistica prevista
039300051020     c     d53lna        oreq      fvalai
039400051020     c     fvaatr        andeq     ' '
039500051020     c     D53tfo        andeq     'A'
039600070103     c* Aggiungo un altro caso: fogli illuminati ma LOCALI inviati come
039700070103     c*  transito per p.o. che sono un po terminal e un po' no
039800070103     c*  (trazioni ridotte es 53 terminal 66, diretto da 05 che nelle
039900070103     c*   trazioni ridotte transita proprio da 66)
040000070103     c     d53lna        oreq      fvalai
040100070103     c     fvaatr        andeq     'T'
040200070103     c     fvaTTR        andeq     'L'
040300070103     c     D53tfo        andeq     'A'
040400031211     c*
040500031211     c* o linea di arrivo = linea invio foglio
040600031211     c*  per i 2 liv in arrivo su altro AS - if che diventa ininfluente
040700031211     c*  quanto si sposteranno su stesso AS
040800051020    2c***  D53LNA        oreq      FVALAI
040900051020     c***  lnatfp        andne     wtfa
041000051020     c***  d53tfo        andeq     'A'
041100051020     c***  wtfa          andeq     WFLP
041200031211     c*
041300031211     c* oppure se sto richiedendo per le bolle
041400051020    2c***  D53LNA        oreq      FVALAI
041500051020     c***  lnatfp        andne     wtfa
041600051020     c***  d53tfo        andeq     'A'
041700051020     C***  D53ABB        andeq     'B'
041800031211     c*
041900031211     c* Aggiungo un caso che serve per sistemare la Sardegna ora
042000031211     c*  ibrida e sbagliata relativamente ai transiti
042100031211     c*  il caso è 82 scarico ( e transito) 113 ma questi colli
042200031211     c*  sono privi di ripartenza dal transito perch 113 non è term
042300031211     c*  partenza
042400040317    2c***  D53LNA        oreq      FVALAI
042500040317     c***  d53tfo        andeq     'A'
042600040317     c***  LNATFP        andeq     FLPTFP
042700040317     C                   MOVEL     'S'           WTROV
042800040317    2C                   ENDIF
042900970430     C*
043000031211     C* Questo CASO E' SOLO PER I FOGLI TRANSITO illuminati
043100031211     c*  relativamente alle bolle
043200970430    2C     WTROV         IFEQ      *BLANKS
043300040511     C     FVAATR        ANDNE     ' '
043400031211     C     D53ABB        ANDeq     'B'
043500031211     C* deve farlo sia per p.o. stesso terminal part e arr
043600031211     c*  ma anche per i p.o. con terminal diverso che vengono sullo
043700031211     c*  stesso ogas del terminal
043800031211    3C***  lnatfp        IFEQ      WTFA
043900970515     C     WTFA          ANDEQ     FVALAI
044000970430     C                   MOVEL     'S'           WTROV
044100031211    3C***                ENDIF
044200970430    2C                   ENDIF
044300970430     C*
044400970430    2C     WTROV         IFEQ      ' '
044500970430     C     D53LAI        ANDEQ     *ZEROS
044600000609     C*
044700031211    3c                   If        §ogntw = 'PPT'
044800031211     C     KFVA2         READE     FNFWA02L                               99
044900031212    4C     *IN99         IFEQ      *OFF
045000031211     C     KFVA          CHAIN     FNFVA01L                           99
045100031211    4C                   ENDIF
045200020122     C*
045300031211   x3C                   ELSE
045400031211     C     KFVA2         READE     FNFVA01L                               99
045500031211    3C                   ENDIF
045600000609     C*
045700031211   x2C                   ELSE
045800031212     C* Accendo il 99 solo se non trovato il foglio di abbinamento
045900031212     c                   if        wtrov=' '
046000031211     C                   SETON                                        99
046100970430    2C                   ENDIF
046200031212    2C                   ENDIF
046300970430     C*
046400970430    1C                   ENDDO
046500970430     C*
046600970430     C     WTROV         IFEQ      *BLANKS
046700970430     C                   MOVE      'N'           WFVAOK
046800970430     C                   END
046900970430     C*
047000970430     C                   ENDSR
047100040511     C**************************************************************************
047200040511     C* CONTROLLO SE una linea di arrivo è relativa al foglio
047300040511     c* impostando FVALAI=terminal di arrivo della linea visto che il campo
047400050418     c*  manca su fnfgv, se non passato altrimenti = al lai passato
047500040511     C**************************************************************************
047600040511     C     CTRFGV        BEGSR
047700040511     C*
047800040511     C                   CLEAR                   WTROV
047900040511     C* IMPOSTO WTFA=0      SE DEVO DETERMINARE IL TER-ARR
048000040511     C* IMPOSTO WTFA=D53TFA SE IL TER-ARR NON E' DA DETERMINARE
048100040511     C     D53TFA        IFEQ      *ZEROS
048200040511     C     D53FGS        IFNE      SAVFGS
048300040511     C     D53LNA        ORNE      SAVLNA
048400040511     C     FgvDFV        ORNE      SAVDFV
048500040511     C                   CLEAR                   WTFA
048600040511     C                   END
048700040511     C                   ELSE
048800040511     C                   MOVE      D53TFA        WTFA
048900040511     C                   END
049000040511     C*
049100040511     C                   MOVE      D53FGS        SAVFGS
049200040511     C                   MOVE      D53LNA        SAVLNA
049300040511     C                   MOVE      FGVDFV        SAVDFV
049400040511     C*
049500040511     C* ESTENSIONE FOGLI PER REPERIRE TUTTE LE LINEE
049600040701     C     kfgw          CHAIN     FNFGW01L                           33
049700040511     C   33              CLEAR                   FgwFF3
049800040511     C   33              CLEAR                   FgwFF4
049900040511     C   33              CLEAR                   FgwFL3
050000040511     C   33              CLEAR                   FgwFL4
050100040511     C*
050200040512     C* se non c'e' determino terminal di arrivo lna
050300040512     C     WTFA          ifEQ      *ZEROS
050400040511     C                   CLEAR                   DSLV55
050500040511     C                   MOVE      D53LNA        D55LIN
050600040511     C                   Z-ADD     FGVDFV        D55DRF
050700040511     C                   MOVE      FGVLNP        D55LNP
050800040511     C*
050900040511     C                   CALL      'FNLV55R'
051000040511     C                   PARM                    DSLV55
051100040511     C                   MOVE      '1'           WLV55             1
051200040511     C                   MOVE      D55TFA        WTFA
051300040511    1C                   END
051400040511     c*
051500040511     C*  CERCO CON LA SUA FILIALE DI SCARICO
051600040511     C                   MOVEL     D53LNA        W003A             3
051700040511     C                   MOVEL     FGVLNA        LNA2G
051800040511     C                   Z-ADD     1             YY                3 0
051900040511     C     W003A         LOOKUP    FFVg(YY)                               33
052000040511     C  N33              CLEAR                   WFLP
052100040511     C   33              MOVEL     FLPg(YY)      WFLP
052200040511     C*
052300050418     c                   if        d53lai=0
052400040512    2C     WFLP          IFEQ      WTFA
052500040511     C                   MOVEL     'S'           WTROV
052600040512     c                   else
052700040511     C*
052800040511     C                   MOVE      'N'           WFVAOK
052900040511     C                   END
053000050418     c
053100050418     c                   else
053200050418     c
053300050418    2C     WFLP          IFEQ      d53lai
053400050418     C                   MOVEL     'S'           WTROV
053500050418     c                   else
053600050418     C*
053700050418     C                   MOVE      'N'           WFVAOK
053800050418     C                   END
053900050418     c                   endif
054000040511     C*
054100040511     C                   ENDSR
054200000000     C**************************************************************************
054300000000     C* R O U T I N E    I N I Z I A L E
054400000000     C**************************************************************************
054500000000     C     *INZSR        BEGSR
054600950130     C*
054700970424     C     KFVA1         KLIST
054800970502     C                   KFLD                    D53FGS
054900970424     C                   KFLD                    D53NFV
055000970424     C                   KFLD                    D53LAI
055100970424     C     KFVA2         KLIST
055200970502     C                   KFLD                    D53FGS
055300970424     C                   KFLD                    D53NFV
055400000427     C     KFWA          KLIST
055500000427     C                   KFLD                    FVALNP
055600000427     C                   KFLD                    FVANFV
055700000427     C                   KFLD                    FVALAI
055800000609     C     KFVA          KLIST
055900000609     C                   KFLD                    FWALNP
056000000609     C                   KFLD                    FWANFV
056100000609     C                   KFLD                    FWALAI
056200970424     C     KFVV1         KLIST
056300970424     C                   KFLD                    D53NPG
056400970424     C                   KFLD                    D53NFV
056500970424     C                   KFLD                    D53FGS
056600020923     C     K02FGV01      klist
056700020923     C                   kfld                    D53NFV
056800040701     C                   kfld                    D53FGS
056900970502     C     KTAB          KLIST
057000970502     C                   KFLD                    CODUT
057100970502     C                   KFLD                    KCOD
057200040701     C     Kfgv          KLIST
057300040701     C                   KFLD                    FgwNFV
057400040701     C                   KFLD                    FgwLNP
057500040701     C     Kfgw          KLIST
057600040701     C                   KFLD                    FgvNFV
057700040701     C                   KFLD                    FgvLNP
057800970430     C*
057900970430     C* DEFINIZIONE DI ALCUNI CAMPI
058000031211     C     *LIKE         DEFINE    d53lna        flptfp
058100031211     C     *LIKE         DEFINE    d53lna        lnatfp
058200031211     C     *LIKE         DEFINE    D53LNA        WFLP
058300970515     C     *LIKE         DEFINE    D53TFA        WTFA
058400970502     C     *LIKE         DEFINE    TBLCOD        KCOD
058500970515     C     *LIKE         DEFINE    D53LNA        LNASAV
058600970602     C     *LIKE         DEFINE    D53LNA        SAVLNA
058700970602     C     *LIKE         DEFINE    D53FGS        SAVFGS
058800020122     C     *LIKE         DEFINE    D53FGS        SA2FGS
058900970602     C     *LIKE         DEFINE    FVADFV        SAVDFV
059000970502     C***
059100970502     C* CARICO I TIPI TRAINO DI DEFLUENZA
059200970502     C***
059300970502     C                   OPEN      TABEL00F
059400970502     C                   Z-ADD     1             CODUT             1 0
059500970502     C                   MOVEL     'TV'          KCOD
059600970502     C                   Z-ADD     1             X                 3 0
059700970502     C     KTAB          SETLL     TABEL
059800970502     C     KTAB          READE     TABEL                                  99
059900970502     C*
060000970502     C     *IN99         DOWEQ     *OFF
060100970502     C     TBLFLG        IFEQ      ' '
060200970502     C                   MOVEL     TBLUNI        DSTV
060300970502     C     §TVDEF        IFEQ      'S'
060400970502     C                   MOVEL     TBLKEY        TTR(X)
060500970502     C                   ADD       1             X
060600970502     C                   ENDIF
060700970502     C                   ENDIF
060800970502     C*
060900970502     C     KTAB          READE     TABEL                                  99
061000970502     C                   ENDDO
061100970502     C                   CLOSE     TABEL00F
061200020122     C**
061300020122     C                   CLEAR                   OG143
061400970430     C*
061500000000     C                   ENDSR
061600031204     C**************************************************************************
061700031204     C* LETTURA FILE FNFGV00F
061800031204     C**************************************************************************
061900031204     c     LEGFGV        begsr
062000040512     C                   MOVE      *BLANKS       WFVAOK            1
062100040701     c
062200040701     c* per p.o. poste leggo fnfgw per cercare il foglio
062300040701    3c                   If        §ogntw = 'PPT'
062400040701     C     Kfva2         CHAIN     FNFGw02L                           99
062500040701     C  n99kfgv          CHAIN     FNFGV01L                           99
062600040701     c                   else
062700031204     C*
062800031204     C     K02FGV01      CHAIN     FNFGV01L                           99
062900040701     c                   endif
063000040701     c
063100031204    1C     *IN99         IFEQ      *OFF
063200031204     C                   MOVEL     1             D53NPG
063300031204     C                   Z-ADD     FGVLNP        D53FGS
063400031204     C                   Z-ADD     FGVLNP        D53FLF
063500031204     C                   Z-ADD     FGVDFV        D53DFV
063600031204     C                   MOVEL     FGVFCF        D53FCF
063700031204     C                   MOVEL     FGVATB        D53ATB
063800031204     C                   MOVEL     FGVTTR        D53TTR
063900031204    2C     FGVTTR        IFNE      *BLANKS
064000031204     C     FGVTTR        LOOKUP    TTR                                    98
064100031204    3C     *IN98         IFEQ      *ON
064200031204     C                   MOVEL     'S'           D53DEF
064300031204    3C                   ENDIF
064400031204    2C                   ENDIF
064500040512     C     D53ABB        ifne      *blanks
064600040512     C                   EXSR      CTRFGV
064700040512    2C                   END
064800031204    1c                   endif
064900031204     c                   endsr
065000031204     C**************************************************************************
065100031204     C* LETTURA FILE FNFVA00F
065200031204     C**************************************************************************
065300031204     c     LEGFVA        BEGSR
065400040112     C                   MOVE      *BLANKS       WFVAOK            1
065500040112
065600031204     C* VERIFICO SE IL P.O. GESTIONE E' POSTE
065700031219    1C**   D53FLF        IFEQ      0
065800031204     C* A CAMBIO DI FGS
065900031219    2C**   D53FGS        IFNE      SA2FGS
066000031219     C**                 CLEAR                   OG143
066100031219     C**   D53FGS        CHAIN     AZORG01L                           30
066200031219     C**N30              MOVEL     ORGDE3        OG143
066300031219     C**                 MOVEL     D53FGS        SA2FGS
066400031219    2C**                 ENDIF
066500031204     C**
066600031219   x1C**                 ELSE
066700031219     C**                 Z-ADD     D53FLF        D53FGS
066800031219    1C**                 ENDIF
066900031204     C**
067000040112
067100031204     C**  FGS = P.O. POSTE --> LEGGO FNFWA02
067200031204    1c                   If        §ogntw = 'PPT'
067300031204     C* C'E' D53LAI
067400031204    2C     D53LAI        IFGT      0
067500031204     C     KFVA1         CHAIN     FNFWA02L                           99
067600031204     C* NON C'E' D53LAI
067700031204     C                   ELSE
067800031204     C     KFVA2         CHAIN     FNFWA02L                           99
067900031204    2C                   ENDIF
068000031204     C**
068100031204   X1C                   ELSE
068200031204     C**
068300031204     C** FGS = NO POSTE
068400031204     C**
068500031204     C* C'E' D53LAI
068600031204    2C     D53LAI        IFGT      0
068700031204     C     KFVA1         CHAIN     FNFVA01L                           99
068800031204     C* NON C'E' D53LAI
068900031204     C                   ELSE
069000031204     C     KFVA2         CHAIN     FNFVA01L                           99
069100031204    2C                   ENDIF
069200031204    1C                   ENDIF
069300031204     c
069400031204     C** 99 off --> foglio  trovato
069500031204    1C     *IN99         IFEQ      *OFF
069600031211     C* SE D53ABB<>' ' CONTROLLO CHE IL FOGLIO LETTO SIA EFFETTIVAMENTE
069700031204     C* QUELLO RELATIVO ALLA LNA PASSATA
069800031204     C*
069900031204    2c                   If        §ogntw = 'PPT'
070000031204     C* LEGGO FVA
070100031204     C     KFVA          CHAIN     FNFVA01L                           99
070200031204    3C     *IN99         IFEQ      *ON
070300040112     C                   MOVEL     'X'           WFVAOK
070400031204    3C                   ENDIF
070500031204    2C                   END
070600031204     C*
070700031204    2C     WFVAOK        IFEQ      *BLANKS
070800031211     C     D53ABB        ANDNE     *blanks
070900031204     C                   EXSR      CTRFVA
071000031204    2C                   END
071100031204     C*
071200031204    2C     WFVAOK        IFEQ      *BLANKS
071300031204     C                   MOVEL     1             D53NPG
071400031204     C                   Z-ADD     FVALNP        D53FGS
071500031204     C                   Z-ADD     FVALNP        D53FLF
071600031204     C                   Z-ADD     FVADFV        D53DFV
071700031204     C                   MOVEL     'S'           D53FCF
071800031204     C                   MOVEL     ' '           D53ATB
071900031204     C                   MOVEL     FVATTR        D53TTR
072000031204     C     FVATTR        IFNE      *BLANKS
072100031204     C     FVATTR        LOOKUP    TTR                                    98
072200031204    3C     *IN98         IFEQ      *ON
072300031204     C                   MOVEL     'S'           D53DEF
072400031204    3C                   ENDIF
072500031204    2C                   ENDIF
072600031204     C                   Z-ADD     FVALAI        D53LAI
072700031204     C                   MOVEL     FVANPG        D53NPA
072800031204     C                   MOVEL     FVANFA        D53NFA
072900031204     C                   MOVEL     FVADFA        D53DFA
073000031204     C                   Z-ADD     FVAAED        D53AED
073100031204     C                   Z-ADD     FVAAEH        D53AEH
073200031204    2C                   ENDIF
073300040112     c* foglio non trovato  WFVAOK=X
073400040112   x1c                   else
073500040112     c                   eval      WFVAOK='X'
073600031204    1C                   ENDIF
073700031204     c                   ENDSR
073800971117**
073900031219Impossibile determinare il tipo di foglio perche' manca la categoria foglio
074000970424Foglio inesistente
074100020919Manca la categoria o P.O. gestione per fogli vari
074200970424Manca il numero del foglio
074300971117Manca la linea di partenza foglio per fogli viaggio partenza in arrivo
074400971117Il foglio non e' relativo alla linea di arrivo di input
074500031204Manca il p.o. gestione foglio
074600031204Non trovato foglio nè in partenza, nè nei fogli partenza in arrivo
074700031204Impossibile richiedere dati di abbinamento per tipo foglio 'J'
074800040512Il p.o. di scarico non è il terminal di arrivo della linea di arrivo
