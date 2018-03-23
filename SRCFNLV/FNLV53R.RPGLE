000100031219     H DECEDIT('0,') DATEDIT(*ymd.)
000200970424     F* FNLV53R *-----------------------------------------------------*
000300970424     F*       REPERIMENTO DATI FOGLIO DA SPUNTE
000400950130     F*---------------------------------------------------------------*
000500970602     FTABEL00F  IF   E           K DISK    USROPN
000600000609     FAZORG01L  IF   E           K DISK
000700080806     FFNFVV01L  IF   E           K DISK    extfile(WlibfV) usropn
000800950130     F*
000900080806     FFNFGV01L  IF   E           K DISK    extfile(WlibfG) usropn
001000080806     FFNFGw02L  IF   E           K DISK    extfile(WlibfW2) usropn
001100040701     F                                     RENAME(FNFgw000:FNFgW002)
001200080806     FFNFVA01L  IF   E           K DISK    extfile(WlibfA) usropn
001300080806     FFNFWA01L  IF   E           K DISK    extfile(WlibfWA) usropn
001400080806     FFNFgw01L  IF   E           K DISK    extfile(WlibfW1) usropn
001500080806     FFNFWA02L  IF   E           K DISK    extfile(WlibfWa2) usropn
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
006900080806     d WLibfV          s             21    inz('FILTRA201 /FNFVV01L')
007000080806     d WLibfG          s             21    inz('FILTRA201 /FNFGV01L')
007100080806     d WLibfW2         s             21    inz('FILTRA201 /FNFGW02L')
007200080806     d WLibfA          s             21    inz('FILTRA201 /FNFVA01L')
007300080806     d WLibfWA         s             21    inz('FILTRA201 /FNFWA01L')
007400080806     d WLibfW1         s             21    inz('FILTRA201 /FNFGW01L')
007500080806     d WLibfWa2        s             21    inz('FILTRA201 /FNFWA02L')
007600970424     C**************************************************************************
007700970424     C     *ENTRY        PLIST
007800970424     C                   PARM                    DSLV53
007900970424     C*
008000970424     C* TIPO LANCIO, D53TLA "C" -           CHIUSO CON LR
008100970424     C* TIPO LANCIO, D53TLA "L" - ELABORO E CHIUDO CON LR
008200970424     C* TIPO LANCIO, D53TLA " " - ELABORO E CHIUDO IN RETRN
008300970424    1C     D53TLA        IFNE      'C'
008400970424     C                   CLEAR                   D53ERR
008500970424     C                   CLEAR                   D53MSG
008600031204     C
008700031204     C* SE NON HO P.O. GESTIONE --> NON POSSO CERCARE IL FOGLIO
008800031204     C                   IF        D53FGS=0
008900031204     C                   MOVEL     MSG(7)        D53MSG
009000031204     C                   MOVEL     '7'           D53ERR
009100031204     C                   GOTO      FINE
009200031204    3C                   ENDIF
009300031204     C* SE MANCA IL NUMERO FOGLIO ERRORE
009400031204    1C     D53NFV        IFEQ      0
009500031204     C                   MOVEL     '4'           D53ERR
009600031204     C                   MOVEL     MSG(4)        D53MSG
009700031204     C                   GOTO      FINE
009800031204    3C                   ENDIF
009900031204     C* SE tipo J e richiesti i dati di abbinamento errore
010000031204    1C     D53TFO        IFEQ      'J'
010100031204     c     D53ABB        andne     *blanks
010200031204     C                   MOVEL     '9'           D53ERR
010300031204     C                   MOVEL     MSG(9)        D53MSG
010400031204     C                   GOTO      FINE
010500031204    3C                   ENDIF
010600031219      *
010700031219      *  Controllo P.O. Gestione passato come parametro
010800031219      *  (se POSTE: come chiave per FNFGV01L devo usare SIMFEL)
010900031219    0c                   if        d53npg=1 or d53tfo<>'V'
011000040701     c***                eval      wD53fgs = D53fgs
011100031219    1C     D53FGS        IFNE      SA2FGS
011200031219     c                   clear                   OG143
011300031219     c     D53fgs        chain     AZORG
011400031219    2c                   if        %found(AZORG01L)
011500031219     c                   movel     ORGDE3        OG143
011600031219    2c                   endif
011700031219     C                   MOVEL     D53FGS        SA2FGS
011800031219    1C                   ENDIF
011900040701    cc
012000040701    1c**                 if        §OGntw  = 'PPT'
012100031219     c* sae poste e manca d53flf --> lo determino
012200040701    2c**                 if        d53flf>0
012300040701     c**                 eval      wD53fgs = D53flf
012400040701   x2c**                 else
012500031219     c* richiamo fnlv55r
012600040701     C**                 CLEAR                   DSLV55
012700040701     C**                 MOVE      D53fgs        D55LIN
012800040701     C**                 Z-ADD     *date         D55DRF
012900040701     C**                 MOVEL     'P'           D55TPT
013000031219     C*
013100040701     C**                 CALL      'FNLV55R'
013200040701     C**                 PARM                    DSLV55
013300040701     c**                 eval      wD53fgs = D55tfp
013400040701    2c**                 endif
013500040701    1c**                 endif
013600031219    0c                   endif
013700970424     C*
013800970424     C** TIPO FOGLIO NON PASSATO --> LO DESUMO
013900970424    2C     D53TFO        IFEQ      ' '
014000970424     C**
014100970424     C* ERRORE SE NON CI SONO TUTTI I DATI NECESSARI
014200970424    3C     D53NPG        IFEQ      0
014300031219     C**   D53FLF        OREQ      0
014400970424     C                   MOVEL     MSG(1)        D53MSG
014500970424     C                   MOVEL     '1'           D53ERR
014600970424     C                   GOTO      FINE
014700970424    3C                   ENDIF
014800970424     C*
014900970424    3C     D53NPG        IFEQ      1
015000031219     C                   MOVEL     'J'           D53TFO
015100970424     C*
015200031219    4C**   D53FLF        IFEQ      D53FEL
015300031219     C**                 MOVEL     'G'           D53TFO
015400031219     C**                 ELSE
015500031219     C**                 MOVEL     'A'           D53TFO
015600031219    4C**                 ENDIF
015700970424     C**
015800970424   X3C                   ELSE
015900970424     C                   MOVEL     'V'           D53TFO
016000970424    3C                   ENDIF
016100970424    2C                   ENDIF
016200970424     C**
016300970424     C                   EXSR      RILFOG
016400970424     C*
016500970424     C     FINE          TAG
016600970424    1C                   ENDIF
016700970424     C* CHIUSURA PGM
016800970424    1C     D53TLA        IFEQ      ' '
016900970424     C                   RETURN
017000970424     C                   ELSE
017100970515     C     WLV55         IFEQ      '1'
017200970515     C                   MOVEL     'C'           D55TLA
017300970515     C                   CALL      'FNLV55R'
017400970515     C                   PARM                    DSLV55
017500970515     C                   END
017600970424     C                   SETON                                        LR
017700970424    1C                   ENDIF
017800970424     C**************************************************************************
017900970424     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI
018000970424     C**************************************************************************
018100970424     C     RILFOG        BEGSR
018200970424     C                   CLEAR                   D53DFV
018300970424     C                   CLEAR                   D53SPG
018400970424     C                   CLEAR                   D53FCF
018500970424     C                   CLEAR                   D53ATB
018600970424     C                   CLEAR                   D53TTR
018700970502     C                   CLEAR                   D53DEF
018800970424     C                   CLEAR                   D53NFA
018900970424     C                   CLEAR                   D53DFA
019000970424     C                   CLEAR                   D53NPA
019100970430     C                   CLEAR                   D53AED
019200970430     C                   CLEAR                   D53AEH
019300970424     C**
019400970424     C* F O G L I O   V I A G G I O   P A R T E N Z A
019500970424     C**
019600031204    1C                   SELECT
019700031204    1C     D53TFO        WHENEQ    'G'
019800031204     c                   EXSR      LEGFGV
019900031204     C* Non trovato foglio
020000031204   X2C                   IF        *IN99
020100970424     C                   MOVEL     '2'           D53ERR
020200970424     C                   MOVEL     MSG(2)        D53MSG
020300040512     c                   else
020400040512     c                   if        WFVAOK='N'
020500040512     C                   MOVEL     '6'           D53ERR
020600040512     C                   MOVEL     MSG(10)       D53MSG
020700031204    2C                   ENDIF
020800040512    2C                   ENDIF
020900970424     C**
021000970424     C* F O G L I O   V I A G G I O   P A R T E N Z A
021100970424     C*               I N   A R R I V O
021200970424     C**
021300031204    1C     D53TFO        WHENEQ    'A'
021400031204     C     D53TFO        OREQ      'T'
021500031204     c                   EXSR      LEGFVA
021600031204     c* Foglio non trovato
021700040112   X2C**                 if        *in99
021800040112     c                   if        WFVAOK='X'
021900970424     C                   MOVEL     '2'           D53ERR
022000970424     C                   MOVEL     MSG(2)        D53MSG
022100031204    2C                   ENDIF
022200031204     c
022300040112    2C**   WFVAOK        IFne      *BLANKS
022400040112     c                   if        WFVAOK='N'
022500031204     C                   MOVEL     '6'           D53ERR
022600031204     C                   MOVEL     MSG(6)        D53MSG
022700031204    2C                   END
022800031204     c**
022900031204     C** D O P P I O   T E N T A T I V O   N E I   F O G L I
023000031204     C**               P A R T E N Z A
023100031204    1C     D53TFO        WHENEQ    'J'
023200031204     C* SE non richiesto l'abbinamento cerco prima in FGV, altrimenti
023300031204     C*  in FVA
023400031204     c     D53ABB        andeq     *blanks
023500031204     c                   EXSR      LEGFGV
023600031204     C* Non trovato foglio
023700031204   X2C                   IF        *IN99
023800031204     c                   EXSR      LEGFVA
023900031204     c* Foglio non trovato
024000040112   X3C**                 if        *in99
024100040112     c                   if        WFVAOK='X'
024200031204     C                   MOVEL     '8'           D53ERR
024300031204     C                   MOVEL     MSG(8)        D53MSG
024400031204    3C                   ENDIF
024500031204    2C                   END
024600031204     c**
024700031204     c**
024800031204    1C**   D53TFO        WHENEQ    'J'
024900031204     C* SE richiesto l'abbinamento cerco prima in FVA, altrimenti
025000031204     C*  in FGV
025100031204     c**   D53ABB        andne     *blanks
025200031204     C* Non trovato foglio
025300031204     c**                 EXSR      LEGFVA
025400031204     c*
025500031204    2C**                 if        not *in99
025600031204    3C**   WFVAOK        IFne      *BLANKS
025700031204     C**                 MOVEL     '6'           D53ERR
025800031204     C**                 MOVEL     MSG(6)        D53MSG
025900031204    3C**                 END
026000031204     c
026100031204   x2c**                 else
026200031204     c
026300031204     c**                 EXSR      LEGFGV
026400031204   X3C**                 if        *in99
026500031204     C**                 MOVEL     '8'           D53ERR
026600031204     C**                 MOVEL     MSG(8)        D53MSG
026700031204    3C**                 ENDIF
026800031204    2C**                 ENDIF
026900031204     c
027000970424     C* F O G L I O   V A R I O
027100970424     C**
027200970424    2C     D53TFO        WHENEQ    'V'
027300970424     C* CI DEVE ESSERE CATEGORIA E P.O. GESTIONE O CATEGORIA E FLE
027400970424    3C     D53FGS        IFGT      0
027500970424     C     D53NPG        ANDGT     0
027600970424     C*
027700020919     C**   D53FLF        ORGT      0
027800020919     C**   D53NPG        ANDGT     0
027900970424     C* SE C'E' LA FILIALE GESTIONE USO QUELLA
028000970424     C* ALTRIMENTI USO LA FILIALE ELABORATORE FOGLIO
028100020917      * Ora riceve sempre il p.o. in gestione
028200970424     C     KFVV1         CHAIN     FNFVV01L                           99
028300970424     C**
028400970424    4C     *IN99         IFEQ      *OFF
028500970424     C                   Z-ADD     FVVFGS        D53FGS
028600970424     C                   Z-ADD     FVVFLE        D53FLF
028700970424     C                   Z-ADD     FVVDFV        D53DFV
028800970424     C                   MOVEL     FVVFCF        D53FCF
028900970424     C                   MOVEL     FVVATB        D53ATB
029000970502     C                   MOVEL     FVVSPG        D53SPG
029100970424   X4C                   ELSE
029200970424     C                   MOVEL     '3'           D53ERR
029300970424     C                   MOVEL     MSG(3)        D53MSG
029400970424    4C                   ENDIF
029500970424     C** ERRORE
029600970424   X3C                   ELSE
029700970424     C                   MOVEL     '3'           D53ERR
029800970424     C                   MOVEL     MSG(3)        D53MSG
029900970424    3C                   ENDIF
030000970424    2C                   ENDSL
030100970424     C*
030200970424     C                   ENDSR
030300970430     C**************************************************************************
030400970430     C* CONTROLLO SE IL FV E' ABBINATO
030500970430     C**************************************************************************
030600031211     C     CTRFVA        BEGSR
030700970430     C*
030800970430     C                   CLEAR                   WTROV
030900970602     C* IMPOSTO WTFA=0      SE DEVO DETERMINARE IL TER-ARR
031000970602     C* IMPOSTO WTFA=D53TFA SE IL TER-ARR NON E' DA DETERMINARE
031100970602     C     D53TFA        IFEQ      *ZEROS
031200970602     C     D53FGS        IFNE      SAVFGS
031300970602     C     D53LNA        ORNE      SAVLNA
031400970602     C     FVADFV        ORNE      SAVDFV
031500970602     C                   CLEAR                   WTFA
031600970602     C                   END
031700970602     C                   ELSE
031800970602     C                   MOVE      D53TFA        WTFA
031900970602     C                   END
032000970602     C*
032100970602     C                   MOVE      D53FGS        SAVFGS
032200970602     C                   MOVE      D53LNA        SAVLNA
032300970602     C                   MOVE      FVADFV        SAVDFV
032400970430     C*
032500031211    1C**   D53LNA        IFEQ      FVALAI
032600031211     C**   D53TFO        ANDEQ     'A'
032700031211     C**                 MOVEL     'S'           WTROV             1
032800031211   X1C**                 ELSE
032900970430     C*
033000020122     C* ESTENSIONE FOGLI PER REPERIRE TUTTE LE LINEE
033100020917     c                   If        §ogntw <> 'PPT'
033200020122     C     KFWA          CHAIN     FNFWA01L                           33
033300020122     C   33              CLEAR                   FWAFF3
033400020122     C   33              CLEAR                   FWAFF4
033500020122     C   33              CLEAR                   FWAFL3
033600020122     C   33              CLEAR                   FWAFL4
033700020122     C                   ENDIF
033800020618     C*
033900020618     C* A CAMBIO LNA O WTFA DETERMINO I TERMINAL DI PART. E ARRIVO
034000031211    1C     D53LNA        IFNE      LNASAV
034100020618     C     WTFA          OREQ      *ZEROS
034200020618     C                   MOVE      D53LNA        LNASAV
034300020618     C                   CLEAR                   DSLV55
034400020618     C                   MOVE      D53LNA        D55LIN
034500020618     C                   Z-ADD     FVADFV        D55DRF
034600031211    2C     WTFA          IFEQ      *ZEROS
034700020618     C                   MOVE      FVALNP        D55LNP
034800031211   X2C                   ELSE
034900020618     C                   MOVEL     'P'           D55TPT
035000031211    2C                   END
035100020618     C*
035200020618     C                   CALL      'FNLV55R'
035300020618     C                   PARM                    DSLV55
035400020618     C                   MOVE      '1'           WLV55             1
035500031211    2C     D55TPT        IFNE      'P'
035600020618     C                   MOVE      D55TFA        WTFA
035700031211    2C                   END
035800031211     c                   eval      lnatfp=D55tfp
035900031211    1C                   END
036000031211     c*
036100031211     C*  CERCO CON LA SUA FILIALE DI SCARICO
036200970430     C                   MOVEL     D53LNA        W003A             3
036300970430     C                   MOVEL     FVALNA        LNA2
036400970430     C                   Z-ADD     1             YY                3 0
036500970430     C     W003A         LOOKUP    FFV(YY)                                33
036600031211     C  N33              CLEAR                   WFLP
036700031211     C   33              MOVEL     FLP(YY)       WFLP
036800031211     c*
036900031211     c* Determino terminal partenza del p.o. scarico
037000031211     C                   CLEAR                   DSLV55
037100031211     C                   MOVE      WFLP          D55LIN
037200031211     C                   Z-ADD     FVADFV        D55DRF
037300031211     C                   MOVEL     'P'           D55TPT
037400031211     C*
037500031211     C                   CALL      'FNLV55R'
037600031211     C                   PARM                    DSLV55
037700031211     C                   MOVE      '1'           WLV55             1
037800031211     c                   eval      flptfp=D55tfp
037900970430     C*
038000031211    1C     *IN99         DOWEQ     *OFF
038100970430     C     WTROV         ANDEQ     *BLANKS
038200970430     C*
038300031211    2C**   D53LNA        IFEQ      FVALAI
038400031211     C**   D53TFO        ANDEQ     'A'
038500031211     C**                 MOVEL     'S'           WTROV
038600031211    2C**                 ENDIF
038700970430     C*
038800031211     c* Per i transiti considero solo p.o. scarico = linea invio foglio
038900031211    2C     WFLP          IFEQ      FVALAI
039000020617     C     D53TFO        ANDEQ     'T'
039100031211     c*
039200031211     c* Per le bolle in arrivo considero p.o. scar = linea invio foglio
039300031211     c*  se è il terminal di arrivo
039400031211    2C     WFLP          OREQ      FVALAI
039500031211     C     WFLP          ANDEQ     WTFA
039600020617     C     D53TFO        ANDEQ     'A'
039700051020     c
039800051020     c* Aggiungo un caso: i fogli diretti che non rientravano in nessuna
039900051020     c*  casistica prevista
040000051020     c     d53lna        oreq      fvalai
040100051020     c     fvaatr        andeq     ' '
040200051020     c     D53tfo        andeq     'A'
040300070103     c* Aggiungo un altro caso: fogli illuminati ma LOCALI inviati come
040400070103     c*  transito per p.o. che sono un po terminal e un po' no
040500070103     c*  (trazioni ridotte es 53 terminal 66, diretto da 05 che nelle
040600070103     c*   trazioni ridotte transita proprio da 66)
040700070103     c     d53lna        oreq      fvalai
040800070103     c     fvaatr        andeq     'T'
040900070103     c     fvaTTR        andeq     'L'
041000070103     c     D53tfo        andeq     'A'
041100031211     c*
041200031211     c* o linea di arrivo = linea invio foglio
041300031211     c*  per i 2 liv in arrivo su altro AS - if che diventa ininfluente
041400031211     c*  quanto si sposteranno su stesso AS
041500051020    2c***  D53LNA        oreq      FVALAI
041600051020     c***  lnatfp        andne     wtfa
041700051020     c***  d53tfo        andeq     'A'
041800051020     c***  wtfa          andeq     WFLP
041900031211     c*
042000031211     c* oppure se sto richiedendo per le bolle
042100051020    2c***  D53LNA        oreq      FVALAI
042200051020     c***  lnatfp        andne     wtfa
042300051020     c***  d53tfo        andeq     'A'
042400051020     C***  D53ABB        andeq     'B'
042500031211     c*
042600031211     c* Aggiungo un caso che serve per sistemare la Sardegna ora
042700031211     c*  ibrida e sbagliata relativamente ai transiti
042800031211     c*  il caso è 82 scarico ( e transito) 113 ma questi colli
042900031211     c*  sono privi di ripartenza dal transito perch 113 non è term
043000031211     c*  partenza
043100040317    2c***  D53LNA        oreq      FVALAI
043200040317     c***  d53tfo        andeq     'A'
043300040317     c***  LNATFP        andeq     FLPTFP
043400040317     C                   MOVEL     'S'           WTROV
043500040317    2C                   ENDIF
043600970430     C*
043700031211     C* Questo CASO E' SOLO PER I FOGLI TRANSITO illuminati
043800031211     c*  relativamente alle bolle
043900970430    2C     WTROV         IFEQ      *BLANKS
044000040511     C     FVAATR        ANDNE     ' '
044100031211     C     D53ABB        ANDeq     'B'
044200031211     C* deve farlo sia per p.o. stesso terminal part e arr
044300031211     c*  ma anche per i p.o. con terminal diverso che vengono sullo
044400031211     c*  stesso ogas del terminal
044500031211    3C***  lnatfp        IFEQ      WTFA
044600970515     C     WTFA          ANDEQ     FVALAI
044700970430     C                   MOVEL     'S'           WTROV
044800031211    3C***                ENDIF
044900970430    2C                   ENDIF
045000970430     C*
045100970430    2C     WTROV         IFEQ      ' '
045200970430     C     D53LAI        ANDEQ     *ZEROS
045300000609     C*
045400031211    3c                   If        §ogntw = 'PPT'
045500031211     C     KFVA2         READE     FNFWA02L                               99
045600031212    4C     *IN99         IFEQ      *OFF
045700031211     C     KFVA          CHAIN     FNFVA01L                           99
045800031211    4C                   ENDIF
045900020122     C*
046000031211   x3C                   ELSE
046100031211     C     KFVA2         READE     FNFVA01L                               99
046200031211    3C                   ENDIF
046300000609     C*
046400031211   x2C                   ELSE
046500031212     C* Accendo il 99 solo se non trovato il foglio di abbinamento
046600031212     c                   if        wtrov=' '
046700031211     C                   SETON                                        99
046800970430    2C                   ENDIF
046900031212    2C                   ENDIF
047000970430     C*
047100970430    1C                   ENDDO
047200970430     C*
047300970430     C     WTROV         IFEQ      *BLANKS
047400970430     C                   MOVE      'N'           WFVAOK
047500970430     C                   END
047600970430     C*
047700970430     C                   ENDSR
047800040511     C**************************************************************************
047900040511     C* CONTROLLO SE una linea di arrivo è relativa al foglio
048000040511     c* impostando FVALAI=terminal di arrivo della linea visto che il campo
048100050418     c*  manca su fnfgv, se non passato altrimenti = al lai passato
048200040511     C**************************************************************************
048300040511     C     CTRFGV        BEGSR
048400040511     C*
048500040511     C                   CLEAR                   WTROV
048600040511     C* IMPOSTO WTFA=0      SE DEVO DETERMINARE IL TER-ARR
048700040511     C* IMPOSTO WTFA=D53TFA SE IL TER-ARR NON E' DA DETERMINARE
048800040511     C     D53TFA        IFEQ      *ZEROS
048900040511     C     D53FGS        IFNE      SAVFGS
049000040511     C     D53LNA        ORNE      SAVLNA
049100040511     C     FgvDFV        ORNE      SAVDFV
049200040511     C                   CLEAR                   WTFA
049300040511     C                   END
049400040511     C                   ELSE
049500040511     C                   MOVE      D53TFA        WTFA
049600040511     C                   END
049700040511     C*
049800040511     C                   MOVE      D53FGS        SAVFGS
049900040511     C                   MOVE      D53LNA        SAVLNA
050000040511     C                   MOVE      FGVDFV        SAVDFV
050100040511     C*
050200040511     C* ESTENSIONE FOGLI PER REPERIRE TUTTE LE LINEE
050300040701     C     kfgw          CHAIN     FNFGW01L                           33
050400040511     C   33              CLEAR                   FgwFF3
050500040511     C   33              CLEAR                   FgwFF4
050600040511     C   33              CLEAR                   FgwFL3
050700040511     C   33              CLEAR                   FgwFL4
050800040511     C*
050900040512     C* se non c'e' determino terminal di arrivo lna
051000040512     C     WTFA          ifEQ      *ZEROS
051100040511     C                   CLEAR                   DSLV55
051200040511     C                   MOVE      D53LNA        D55LIN
051300040511     C                   Z-ADD     FGVDFV        D55DRF
051400040511     C                   MOVE      FGVLNP        D55LNP
051500040511     C*
051600040511     C                   CALL      'FNLV55R'
051700040511     C                   PARM                    DSLV55
051800040511     C                   MOVE      '1'           WLV55             1
051900040511     C                   MOVE      D55TFA        WTFA
052000040511    1C                   END
052100040511     c*
052200040511     C*  CERCO CON LA SUA FILIALE DI SCARICO
052300040511     C                   MOVEL     D53LNA        W003A             3
052400040511     C                   MOVEL     FGVLNA        LNA2G
052500040511     C                   Z-ADD     1             YY                3 0
052600040511     C     W003A         LOOKUP    FFVg(YY)                               33
052700040511     C  N33              CLEAR                   WFLP
052800040511     C   33              MOVEL     FLPg(YY)      WFLP
052900040511     C*
053000050418     c                   if        d53lai=0
053100040512    2C     WFLP          IFEQ      WTFA
053200040511     C                   MOVEL     'S'           WTROV
053300040512     c                   else
053400040511     C*
053500040511     C                   MOVE      'N'           WFVAOK
053600040511     C                   END
053700050418     c
053800050418     c                   else
053900050418     c
054000050418    2C     WFLP          IFEQ      d53lai
054100050418     C                   MOVEL     'S'           WTROV
054200050418     c                   else
054300050418     C*
054400050418     C                   MOVE      'N'           WFVAOK
054500050418     C                   END
054600050418     c                   endif
054700040511     C*
054800040511     C                   ENDSR
054900000000     C**************************************************************************
055000000000     C* R O U T I N E    I N I Z I A L E
055100000000     C**************************************************************************
055200000000     C     *INZSR        BEGSR
055300950130     C*
055400970424     C     KFVA1         KLIST
055500970502     C                   KFLD                    D53FGS
055600970424     C                   KFLD                    D53NFV
055700970424     C                   KFLD                    D53LAI
055800970424     C     KFVA2         KLIST
055900970502     C                   KFLD                    D53FGS
056000970424     C                   KFLD                    D53NFV
056100000427     C     KFWA          KLIST
056200000427     C                   KFLD                    FVALNP
056300000427     C                   KFLD                    FVANFV
056400000427     C                   KFLD                    FVALAI
056500000609     C     KFVA          KLIST
056600000609     C                   KFLD                    FWALNP
056700000609     C                   KFLD                    FWANFV
056800000609     C                   KFLD                    FWALAI
056900970424     C     KFVV1         KLIST
057000970424     C                   KFLD                    D53NPG
057100970424     C                   KFLD                    D53NFV
057200970424     C                   KFLD                    D53FGS
057300020923     C     K02FGV01      klist
057400020923     C                   kfld                    D53NFV
057500040701     C                   kfld                    D53FGS
057600970502     C     KTAB          KLIST
057700970502     C                   KFLD                    CODUT
057800970502     C                   KFLD                    KCOD
057900040701     C     Kfgv          KLIST
058000040701     C                   KFLD                    FgwNFV
058100040701     C                   KFLD                    FgwLNP
058200040701     C     Kfgw          KLIST
058300040701     C                   KFLD                    FgvNFV
058400040701     C                   KFLD                    FgvLNP
058500970430     C*
058600970430     C* DEFINIZIONE DI ALCUNI CAMPI
058700031211     C     *LIKE         DEFINE    d53lna        flptfp
058800031211     C     *LIKE         DEFINE    d53lna        lnatfp
058900031211     C     *LIKE         DEFINE    D53LNA        WFLP
059000970515     C     *LIKE         DEFINE    D53TFA        WTFA
059100970502     C     *LIKE         DEFINE    TBLCOD        KCOD
059200970515     C     *LIKE         DEFINE    D53LNA        LNASAV
059300970602     C     *LIKE         DEFINE    D53LNA        SAVLNA
059400970602     C     *LIKE         DEFINE    D53FGS        SAVFGS
059500020122     C     *LIKE         DEFINE    D53FGS        SA2FGS
059600970602     C     *LIKE         DEFINE    FVADFV        SAVDFV
059700970502     C***
059800970502     C* CARICO I TIPI TRAINO DI DEFLUENZA
059900970502     C***
060000970502     C                   OPEN      TABEL00F
060100970502     C                   Z-ADD     1             CODUT             1 0
060200970502     C                   MOVEL     'TV'          KCOD
060300970502     C                   Z-ADD     1             X                 3 0
060400970502     C     KTAB          SETLL     TABEL
060500970502     C     KTAB          READE     TABEL                                  99
060600970502     C*
060700970502     C     *IN99         DOWEQ     *OFF
060800970502     C     TBLFLG        IFEQ      ' '
060900970502     C                   MOVEL     TBLUNI        DSTV
061000970502     C     §TVDEF        IFEQ      'S'
061100970502     C                   MOVEL     TBLKEY        TTR(X)
061200970502     C                   ADD       1             X
061300970502     C                   ENDIF
061400970502     C                   ENDIF
061500970502     C*
061600970502     C     KTAB          READE     TABEL                                  99
061700970502     C                   ENDDO
061800970502     C                   CLOSE     TABEL00F
061900020122     C**
062000020122     C                   CLEAR                   OG143
062100970430     C*
062200080806      * APRO FILE
062300080806     c
062400080806     c                   open(e)   FNFVV01L
062500080806     c                   if        not %open(FNFVV01L)
062600080806     c                   eval      %subst(WlibfV:7:3)='PRD'
062700080806     c                   eval      %subst(WlibfG :7:3)='PRD'
062800080806     c                   eval      %subst(WlibfW2 :7:3)='PRD'
062900080806     c                   eval      %subst(WlibfA :7:3)='PRD'
063000080806     c                   eval      %subst(WlibfWA:7:3)='PRD'
063100080806     c                   eval      %subst(WlibfW1:7:3)='PRD'
063200080806     c                   eval      %subst(WlibfWa2:7:3)='PRD'
063300080806     c
063400080806     c                   open      FNFVV01L
063500080806     c                   endif
063600080806     c
063700080806     c                   open      FNFGV01L
063800080806     c                   open      FNFGW02L
063900080806     c                   open      FNFVA01L
064000080806     c                   open      FNFWA01L
064100080806     c                   open      FNFGW01L
064200080806     c                   open      FNFWA02L
064300080806     c
064400000000     C                   ENDSR
064500031204     C**************************************************************************
064600031204     C* LETTURA FILE FNFGV00F
064700031204     C**************************************************************************
064800031204     c     LEGFGV        begsr
064900040512     C                   MOVE      *BLANKS       WFVAOK            1
065000040701     c
065100040701     c* per p.o. poste leggo fnfgw per cercare il foglio
065200040701    3c                   If        §ogntw = 'PPT'
065300040701     C     Kfva2         CHAIN     FNFGw02L                           99
065400040701     C  n99kfgv          CHAIN     FNFGV01L                           99
065500040701     c                   else
065600031204     C*
065700031204     C     K02FGV01      CHAIN     FNFGV01L                           99
065800040701     c                   endif
065900040701     c
066000031204    1C     *IN99         IFEQ      *OFF
066100031204     C                   MOVEL     1             D53NPG
066200031204     C                   Z-ADD     FGVLNP        D53FGS
066300031204     C                   Z-ADD     FGVLNP        D53FLF
066400031204     C                   Z-ADD     FGVDFV        D53DFV
066500031204     C                   MOVEL     FGVFCF        D53FCF
066600031204     C                   MOVEL     FGVATB        D53ATB
066700031204     C                   MOVEL     FGVTTR        D53TTR
066800031204    2C     FGVTTR        IFNE      *BLANKS
066900031204     C     FGVTTR        LOOKUP    TTR                                    98
067000031204    3C     *IN98         IFEQ      *ON
067100031204     C                   MOVEL     'S'           D53DEF
067200031204    3C                   ENDIF
067300031204    2C                   ENDIF
067400040512     C     D53ABB        ifne      *blanks
067500040512     C                   EXSR      CTRFGV
067600040512    2C                   END
067700031204    1c                   endif
067800031204     c                   endsr
067900031204     C**************************************************************************
068000031204     C* LETTURA FILE FNFVA00F
068100031204     C**************************************************************************
068200031204     c     LEGFVA        BEGSR
068300040112     C                   MOVE      *BLANKS       WFVAOK            1
068400040112
068500031204     C* VERIFICO SE IL P.O. GESTIONE E' POSTE
068600031219    1C**   D53FLF        IFEQ      0
068700031204     C* A CAMBIO DI FGS
068800031219    2C**   D53FGS        IFNE      SA2FGS
068900031219     C**                 CLEAR                   OG143
069000031219     C**   D53FGS        CHAIN     AZORG01L                           30
069100031219     C**N30              MOVEL     ORGDE3        OG143
069200031219     C**                 MOVEL     D53FGS        SA2FGS
069300031219    2C**                 ENDIF
069400031204     C**
069500031219   x1C**                 ELSE
069600031219     C**                 Z-ADD     D53FLF        D53FGS
069700031219    1C**                 ENDIF
069800031204     C**
069900040112
070000031204     C**  FGS = P.O. POSTE --> LEGGO FNFWA02
070100031204    1c                   If        §ogntw = 'PPT'
070200031204     C* C'E' D53LAI
070300031204    2C     D53LAI        IFGT      0
070400031204     C     KFVA1         CHAIN     FNFWA02L                           99
070500031204     C* NON C'E' D53LAI
070600031204     C                   ELSE
070700031204     C     KFVA2         CHAIN     FNFWA02L                           99
070800031204    2C                   ENDIF
070900031204     C**
071000031204   X1C                   ELSE
071100031204     C**
071200031204     C** FGS = NO POSTE
071300031204     C**
071400031204     C* C'E' D53LAI
071500031204    2C     D53LAI        IFGT      0
071600031204     C     KFVA1         CHAIN     FNFVA01L                           99
071700031204     C* NON C'E' D53LAI
071800031204     C                   ELSE
071900031204     C     KFVA2         CHAIN     FNFVA01L                           99
072000031204    2C                   ENDIF
072100031204    1C                   ENDIF
072200031204     c
072300031204     C** 99 off --> foglio  trovato
072400031204    1C     *IN99         IFEQ      *OFF
072500031211     C* SE D53ABB<>' ' CONTROLLO CHE IL FOGLIO LETTO SIA EFFETTIVAMENTE
072600031204     C* QUELLO RELATIVO ALLA LNA PASSATA
072700031204     C*
072800031204    2c                   If        §ogntw = 'PPT'
072900031204     C* LEGGO FVA
073000031204     C     KFVA          CHAIN     FNFVA01L                           99
073100031204    3C     *IN99         IFEQ      *ON
073200040112     C                   MOVEL     'X'           WFVAOK
073300031204    3C                   ENDIF
073400031204    2C                   END
073500031204     C*
073600031204    2C     WFVAOK        IFEQ      *BLANKS
073700031211     C     D53ABB        ANDNE     *blanks
073800031204     C                   EXSR      CTRFVA
073900031204    2C                   END
074000031204     C*
074100031204    2C     WFVAOK        IFEQ      *BLANKS
074200031204     C                   MOVEL     1             D53NPG
074300031204     C                   Z-ADD     FVALNP        D53FGS
074400031204     C                   Z-ADD     FVALNP        D53FLF
074500031204     C                   Z-ADD     FVADFV        D53DFV
074600031204     C                   MOVEL     'S'           D53FCF
074700031204     C                   MOVEL     ' '           D53ATB
074800031204     C                   MOVEL     FVATTR        D53TTR
074900031204     C     FVATTR        IFNE      *BLANKS
075000031204     C     FVATTR        LOOKUP    TTR                                    98
075100031204    3C     *IN98         IFEQ      *ON
075200031204     C                   MOVEL     'S'           D53DEF
075300031204    3C                   ENDIF
075400031204    2C                   ENDIF
075500031204     C                   Z-ADD     FVALAI        D53LAI
075600031204     C                   MOVEL     FVANPG        D53NPA
075700031204     C                   MOVEL     FVANFA        D53NFA
075800031204     C                   MOVEL     FVADFA        D53DFA
075900031204     C                   Z-ADD     FVAAED        D53AED
076000031204     C                   Z-ADD     FVAAEH        D53AEH
076100031204    2C                   ENDIF
076200040112     c* foglio non trovato  WFVAOK=X
076300040112   x1c                   else
076400040112     c                   eval      WFVAOK='X'
076500031204    1C                   ENDIF
076600031204     c                   ENDSR
076700971117**
076800031219Impossibile determinare il tipo di foglio perche' manca la categoria foglio
076900970424Foglio inesistente
077000020919Manca la categoria o P.O. gestione per fogli vari
077100970424Manca il numero del foglio
077200971117Manca la linea di partenza foglio per fogli viaggio partenza in arrivo
077300971117Il foglio non e' relativo alla linea di arrivo di input
077400031204Manca il p.o. gestione foglio
077500031204Non trovato foglio nè in partenza, nè nei fogli partenza in arrivo
077600031204Impossibile richiedere dati di abbinamento per tipo foglio 'J'
077700040512Il p.o. di scarico non è il terminal di arrivo della linea di arrivo
